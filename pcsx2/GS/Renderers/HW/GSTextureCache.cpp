/*  PCSX2 - PS2 Emulator for PCs
 *  Copyright (C) 2002-2021 PCSX2 Dev Team
 *
 *  PCSX2 is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License as published by the Free Software Found-
 *  ation, either version 3 of the License, or (at your option) any later version.
 *
 *  PCSX2 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along with PCSX2.
 *  If not, see <http://www.gnu.org/licenses/>.
 */

#include "PrecompiledHeader.h"
#include "GSTextureCache.h"
#include "GSRendererHW.h"
#include "GS/GSGL.h"
#include "GS/GSIntrin.h"
#include "GS/GSUtil.h"

GSTextureCache::GSTextureCache(GSRenderer* r)
	: m_renderer(r)
	, m_palette_map(r)
{
	if (theApp.GetConfigB("UserHacks"))
	{
		UserHacks_HalfPixelOffset = theApp.GetConfigI("UserHacks_HalfPixelOffset") == 1;
	}
	else
	{
		UserHacks_HalfPixelOffset = false;
	}

	m_paltex = theApp.GetConfigB("paltex");
	m_crc_hack_level = theApp.GetConfigT<CRCHackLevel>("crc_hack_level");
	if (m_crc_hack_level == CRCHackLevel::Automatic)
		m_crc_hack_level = GSUtil::GetRecommendedCRCHackLevel(theApp.GetCurrentRendererType());

	// In theory 4MB is enough but 9MB is safer for overflow (8MB
	// isn't enough in custom resolution)
	// Test: onimusha 3 PAL 60Hz
	m_temp = (u8*)_aligned_malloc(9 * 1024 * 1024, 32);
}

GSTextureCache::~GSTextureCache()
{
	RemoveAll();

	_aligned_free(m_temp);
}

void GSTextureCache::RemoveAll()
{
	if (m_renderer->m_dev)
	{
		GL_INS("TC: RemoveAll.");
		constexpr u32 bp = 0;
		constexpr u32 bw = 1;
		constexpr u32 psm = PSM_PSMCT32;
		const GSOffset& off = m_renderer->m_mem.GetOffset(bp, bw, psm);
		const GSVector2i s = GSLocalMemory::m_psm[psm].pgs;
		const GSVector4i r = GSVector4i(0, 0, s.x, s.y * MAX_PAGES);
		for (const PageInfo& pi : m_pages)
		{
			assert(!pi.fb || m_surfaces.find(pi.fb) != m_surfaces.cend());
			for (Surface* s : pi.copies)
				assert(m_surfaces.find(s) != m_surfaces.cend());
		}
		InvalidateLocalMem(off, r); // Read back whole memory.
		GL_INS("TC: Fully invalidated local memory.");
		for (const PageInfo& pi : m_pages)
		{
			assert(pi.state == PageState::CPU);
		}
		InvalidateVideoMem(off, r, nullptr); // Clear cache.
		GL_INS("TC: Fully invalidated video memory.");
		for (const PageInfo& pi : m_pages)
		{
			assert(pi.state == PageState::CPU);
			assert(pi.fb == nullptr);
			assert(!pi.copies.size());
		}
	}
	else
	{
		printf("TC: RemoveAll - Cannot readback\n");
	}

	// Delete surfaces.
	for (Surface* s : m_surfaces)
		delete s;
	m_surfaces.clear();

	m_palette_map.Clear();
}

GSTextureCache::Surface* GSTextureCache::LookupSource(const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, const GSVector4i& r)
{
	if (!TEX0.TBW || !TEX0.TW || !TEX0.TH)
		return nullptr;
	ASSERT(TEX0.TBW > 0);
	ASSERT(TEX0.TBP0 < 0x4000);
	ASSERT(TEX0.PSM < 64);
	ASSERT((r >= GSVector4i(0, 0, 0, 0)).alltrue());

	const GSLocalMemory::psm_t& psm_s = GSLocalMemory::m_psm[TEX0.PSM];
	//const GSLocalMemory::psm_t& cpsm = psm.pal > 0 ? GSLocalMemory::m_psm[TEX0.CPSM] : psm;

	// Until DX is fixed
	if (psm_s.pal > 0)
		m_renderer->m_mem.m_clut.Read32(TEX0, TEXA);

	const u32* clut = m_renderer->m_mem.m_clut;

	Surface* src = nullptr;

	const u32 page = TEX0.TBP0 >> 5;

	auto& m = m_pages.at(page).copies;

	for (auto i = m.begin(); i != m.end(); ++i)
	{
		Surface* s = *i;

		assert(m_surfaces.find(s) != m_surfaces.cend());

		if (((TEX0.U32[0] ^ s->m_TEX0.U32[0]) | ((TEX0.U32[1] ^ s->m_TEX0.U32[1]) & 3)) != 0) // TBP0 TBW PSM TW TH
			continue;

		// TODO Check readback.
		// We request a palette texture (psm_s.pal). If the texture was
		// converted by the CPU (!s->m_palette), we need to ensure
		// palette content is the same.
		if (psm_s.pal > 0 && !s->GetPalette() && !s->ClutMatch({clut, psm_s.pal}))
			continue;

		// We request a 24/16 bit RGBA texture. Alpha expansion was done by
		// the CPU.  We need to check that TEXA is identical
		if (psm_s.pal == 0 && psm_s.fmt > 0 && s->m_TEXA.U64 != TEXA.U64)
			continue;

		m.MoveFront(i.Index());

		src = s;

		break;
	}

	const bool miss = src == nullptr;
	if (miss)
	{
		GL_CACHE("TC: src miss (0x%X, 0x%X, %s)", TEX0.TBP0, psm_s.pal > 0 ? TEX0.CBP : 0, psm_str(TEX0.PSM));
		src = new Surface(m_renderer, m_temp, TEX0, TEXA, m_palette_map, m_paltex, r, SurfaceType::Source);
		assert(m_surfaces.find(src) == m_surfaces.cend());
		m_surfaces.insert(src);
	}
	else
	{
		GL_CACHE("TC: src hit: %d (0x%X, 0x%X, %s)",
			src->GetTexture() ? src->GetTexture()->GetID() : 0,
			TEX0.TBP0, psm_s.pal > 0 ? TEX0.CBP : 0,
			psm_str(TEX0.PSM));
		if (src->GetPalette() && !src->ClutMatch({clut, psm_s.pal}))
			src->AttachPalette(m_palette_map);
		src->Extend(TEX0);
	}
	UpdateSurface(src, r);
	if (miss)
		src->m_off.pageLooperForRect(r).loopPages([&](const u32 p) {
			if (src->GetPageValidity(p))
				m_pages.at(p).copies.InsertFront(src);
		});
	return src;
}

GSTextureCache::Surface* GSTextureCache::LookupTarget(const GIFRegTEX0& TEX0, const GSVector4i& r, const SurfaceType type)
{
	if (!TEX0.TBW)
		return nullptr;
	ASSERT(type == SurfaceType::RenderTarget || type == SurfaceType::DepthStencil);
	ASSERT(TEX0.TBW > 0);
	ASSERT(TEX0.TBP0 < 0x4000);
	ASSERT(TEX0.PSM < 64);
	ASSERT(!TEX0.TW);
	ASSERT(!TEX0.TH);
	const u32 page = TEX0.TBP0 >> 5;
	const PageInfo& pi = m_pages.at(page);
	Surface* t = nullptr;
	if (pi.fb && pi.fb->m_type == type && pi.fb->m_TEX0.PSM == TEX0.PSM && pi.fb->m_TEX0.TBW == TEX0.TBW && pi.fb->m_TEX0.TBP0 == TEX0.TBP0)
	{
		// Recycle pi.fb.
		t = pi.fb;
		t->Extend(r);
		GL_CACHE("TC: dst hit (0x%X, %s, FbAddr: 0x%08X, FbTexID: %d)", TEX0.TBP0, psm_str(TEX0.PSM), t, t->GetTexture() ? t->GetTexture()->GetID() : -1);
	}
	else
	{
		// Create new target.
		t = new Surface(m_renderer, m_temp, TEX0, {}, m_palette_map, m_paltex, r, type);
		ASSERT(t);
		ASSERT(m_surfaces.find(t) == m_surfaces.cend());
		m_surfaces.emplace(t);
		GL_CACHE("TC: dst miss (0x%X, %s)", TEX0.TBP0, psm_str(TEX0.PSM));
	}
	ASSERT(t);
	UpdateSurface(t, r);
	return t;
}

void GSTextureCache::Read(Surface* s, const GSVector4i& r)
{
	assert(s);
	assert(s->m_type != SurfaceType::Invalid && s->m_type != SurfaceType::Source);

	if (r.width() == 0 || r.height() == 0)
		return;

	const GIFRegTEX0& TEX0 = s->m_TEX0;

	GSTexture::Format fmt;
	ShaderConvert ps_shader;
	switch (TEX0.PSM)
	{
		case PSM_PSMCT32:
		case PSM_PSMCT24:
			fmt = GSTexture::Format::Color;
			ps_shader = ShaderConvert::COPY;
			break;

		case PSM_PSMCT16:
		case PSM_PSMCT16S:
			fmt = GSTexture::Format::UInt16;
			ps_shader = ShaderConvert::RGBA8_TO_16_BITS;
			break;

		case PSM_PSMZ32:
		case PSM_PSMZ24:
			fmt = GSTexture::Format::UInt32;
			ps_shader = ShaderConvert::FLOAT32_TO_32_BITS;
			break;

		case PSM_PSMZ16:
		case PSM_PSMZ16S:
			fmt = GSTexture::Format::UInt16;
			ps_shader = ShaderConvert::FLOAT32_TO_32_BITS;
			break;

		default:
			return;
	}

	// Yes lots of logging, but I'm not confident with this code
	GL_PUSH("Texture Cache Read. Format(0x%x)", TEX0.PSM);

	GL_PERF("TC: Read Back Target: %d (0x%x)[fmt: 0x%x]. Size %dx%d",
		s->GetTexture()->GetID(), TEX0.TBP0, TEX0.PSM, r.width(), r.height());

	const GSVector4 src = GSVector4(r) * GSVector4(s->GetTexture()->GetScale()).xyxy() / GSVector4(s->GetTexture()->GetSize()).xyxy();

	bool res;
	GSTexture::GSMap m;

	if (s->GetTexture()->GetScale() == GSVector2(1, 1) && ps_shader == ShaderConvert::COPY)
		res = m_renderer->m_dev->DownloadTexture(s->GetTexture(), r, m);
	else
		res = m_renderer->m_dev->DownloadTextureConvert(s->GetTexture(), src, GSVector2i(r.width(), r.height()), fmt, ps_shader, m, false);

	if (res)
	{
		const GSOffset off = m_renderer->m_mem.GetOffset(TEX0.TBP0, TEX0.TBW, TEX0.PSM);

		switch (TEX0.PSM)
		{
			case PSM_PSMCT32:
			case PSM_PSMZ32:
				m_renderer->m_mem.WritePixel32(m.bits, m.pitch, off, r);
				break;
			case PSM_PSMCT24:
			case PSM_PSMZ24:
				m_renderer->m_mem.WritePixel24(m.bits, m.pitch, off, r);
				break;
			case PSM_PSMCT16:
			case PSM_PSMCT16S:
			case PSM_PSMZ16:
			case PSM_PSMZ16S:
				m_renderer->m_mem.WritePixel16(m.bits, m.pitch, off, r);
				break;

			default:
				ASSERT(0);
		}

		m_renderer->m_dev->DownloadTextureComplete();
	}
}


// Goal: invalidate data sent to the GPU when the source (GS memory) is modified
// Called each time you want to write to the GS memory
void GSTextureCache::InvalidateVideoMem(const GSOffset& off, const GSVector4i& rect, Surface* fb)
{
	assert(!fb || IsFb(fb->m_type));
	assert(!fb || m_surfaces.find(fb) != m_surfaces.cend()); // fb is null or in m_surfaces.
	assert(!fb || off == fb->m_off); // fb is null or same offset.

	// Invalidate pages.
	off.pageLooperForRect(rect).loopPages([&](const u32 p) {
		PageInfo& pi = m_pages.at(p);
		// Invalidate Source(s).
		for (Surface* s : pi.copies)
		{
			assert(s);
			assert(m_surfaces.find(s) != m_surfaces.cend());
			assert(s->GetPageValidity(p));
			s->SetPageValidity(p, 0);
			if (s->IsFullyDirty())
			{
				m_surfaces.erase(s);
				delete s;
			}
		}
		pi.copies.clear();

		// Invalidate Target.
		Surface* old_fb = pi.fb;
		assert(!old_fb || !old_fb->IsDirtyPage(p));
		assert(!fb || !fb->IsDirtyPage(p));
		if (fb != old_fb)
		{
			// GL_INS("Page invalidated on GPU: #%u, %s", p, pi.to_string().c_str());
			if (old_fb)
			{
				assert(m_surfaces.find(old_fb) != m_surfaces.cend());
				// Set page as dirty in old_fb.
				old_fb->SetPageValidity(p, 0);
				pi.fb = nullptr; // Remove reference.
				if (old_fb->IsFullyDirty())
				{
					// Delete target if it has no valid page.
					m_surfaces.erase(old_fb); // Remove from tracking set.
					delete old_fb;
				}
			}
			assert(!pi.fb);
			pi.fb = fb; // Replace fb.
			pi.state = fb ? PageState::GPU : PageState::CPU; // Update state.
			// GL_INS("New page state: #%u, %s", p, pi.to_string().c_str());
		}
		assert(!pi.fb || !pi.fb->IsDirtyPage(p));
	});
}

// Goal: retrive the data from the GPU to the GS memory.
// Called each time you want to read from the GS memory
void GSTextureCache::InvalidateLocalMem(const GSOffset& off, const GSVector4i& r)
{
	// Readback pages.
	u32 count = 0;
	off.pageLooperForRect(r).loopPages([&](const u32 p) {
		++count;
		// GL_INS("TC: InvalidateLocalMem %d page.", p);
		PageInfo& pi = m_pages.at(p);
		if (pi.state == PageState::GPU)
		{
			assert(pi.fb);
			assert(!pi.fb->IsDirtyPage(p));
			ASSERT(m_surfaces.find(pi.fb) != m_surfaces.cend()); // fb is null or in m_dst.
			// GL_INS("Page to be readback from GPU: #%u, %s", p, pi.to_string().c_str());
			const GSVector4i rp = pi.fb->m_off.GetRect(p);
			// TODO Detect and record page column overflow.
			Read(pi.fb, rp); // TODO Slow! Merge multiple small reads when possible.
			pi.state = PageState::CPU;
			// pi.fb still valid.
			assert(pi.is_sync());
		}
	});
	// GL_INS("TC: InvalidateLocalMem %d pages.", count);
}

void GSTextureCache::UpdateSurfacePage(Surface* s, const Surface* t, const u32 p, bool& out_result)
{
	// Copy page p from t to s.
	assert(s->IsDirtyPage(p)); // Page is dirty in s.
	assert(!t->IsDirtyPage(p)); // Page is clean in t.
	assert(s && s->GetTexture());
	assert(t && t->GetTexture());
	out_result = false;

	constexpr bool enableDirectGPUCopyConvert = false;
	if (!enableDirectGPUCopyConvert)
		return;

	const u32 psm_s = s->m_TEX0.PSM;
	const u32 psm_t = t->m_TEX0.PSM;

	if (psm_s == PSM_PSMT4)
	{
		// There is no shader code to convert 4 bits.
		// Read it back for now.
		// GL_INS("ERROR - TC - UpdateSurfacePage - unsupported PSM_PSMT4.");
		return;
	}

	const bool is_pal_t = GSLocalMemory::m_psm[psm_t].pal != 0;
	if (t->m_type == SurfaceType::Source && is_pal_t)
	{
		GL_INS("ERROR - TC - UpdateSurfacePage - unsupported PALETTED-SOURCE-COPY.");
		return;
	}

	const u32 bp_t = t->m_TEX0.TBP0;
	const u32 bp_s = s->m_TEX0.TBP0;
	if (p == t->GetBlockPointerPage() && bp_t != bp_s)
	{
		// TODO Handle delicate case.
		// GL_INS("ERROR - TC - UpdateSurfacePage - unsupported OFFSET-COPY.");
		return;
	}

	const GSVector4i p_rect_t = t->m_off.GetRect(p);
	const GSVector4i p_rect_s = s->m_off.GetRect(p);
	if (!p_rect_s.rintersect(s->m_rect).eq(p_rect_s))
	{
		// The page is not fully covered by the source texture.
		// Read it back for now.
		// TODO Handle delicate case.
		// GL_INS("ERROR - TC - UpdateSurfacePage - unsupported PARTIAL-PAGE-COPY.");
		return;
	}

	const bool linear = psm_s == PSM_PSMCT32 || psm_s == PSM_PSMCT24;
	GSTexture* sTex = t->GetTexture(); // Copy source.
	GSTexture* dTex = s->GetTexture(); // Copy destination.
	const int w_t = t->m_rect.width();
	const int h_t = t->m_rect.height();
	const GSVector4 size_t = GSVector4(w_t, h_t).xyxy();
	ASSERT((size_t == GSVector4(w_t, h_t, w_t, h_t)).alltrue());

	if (psm_s == psm_t && (p_rect_s == p_rect_t).alltrue())
	{
		// Copy.
		const GSVector4i r = p_rect_s;
		m_renderer->m_dev->CopyRect(sTex, dTex, r);
		out_result = true;
		return;
	}

	if (psm_s == psm_t)
	{
		// Copy.
		const GSVector4 sRect = GSVector4(p_rect_t) / size_t;
		const GSVector4 dRect = GSVector4(p_rect_s) * s->GetScale();
		constexpr ShaderConvert shader = ShaderConvert::COPY;
		m_renderer->m_dev->StretchRect(sTex, sRect, dTex, dRect, shader, linear);
		out_result = true;
		return;
	}

	// Copy convert.
	if (s->m_type == SurfaceType::RenderTarget && t->m_type == SurfaceType::DepthStencil)
	{
		ShaderConvert shader;
		bool fmt_16_bits = (GSLocalMemory::m_psm[psm_s].bpp == 16 &&
							GSLocalMemory::m_psm[psm_t].bpp == 16);
		if (s->m_type == SurfaceType::DepthStencil)
		{
			shader = (fmt_16_bits) ? ShaderConvert::RGB5A1_TO_FLOAT16 : (ShaderConvert)((int)ShaderConvert::RGBA8_TO_FLOAT32 + GSLocalMemory::m_psm[psm_s].fmt);
		}
		else
		{
			shader = (fmt_16_bits) ? ShaderConvert::FLOAT16_TO_RGB5A1 : ShaderConvert::FLOAT32_TO_RGBA8;
		}
		const GSVector4 sRect = GSVector4(p_rect_t) / size_t;
		const GSVector4 dRect = GSVector4(p_rect_s) * s->GetScale();
		m_renderer->m_dev->StretchRect(t->GetTexture(), sRect, s->GetTexture(), dRect, shader, false);
		out_result = true;
		return;
	}

	const bool is_8bits = psm_s == PSM_PSMT8;
	const ShaderConvert shader = is_8bits ? ShaderConvert::RGBA_TO_8I : (s->m_type == t->m_type || t->m_type == SurfaceType::RenderTarget) ? ShaderConvert::COPY :
                                                                                                                                             ShaderConvert::FLOAT32_TO_RGBA8;

	if (is_8bits)
	{
		GL_INS("Reading RT as a packed-indexed 8 bits format");
	}

	const u32 tw = p_rect_s.width();
	const u32 th = p_rect_s.height();

	// do not round here!!! if edge becomes a black pixel and addressing mode is clamp => everything outside the clamped area turns into black (kh2 shadows)

	int w = (int)(s->GetTexture()->GetScale().x * tw);
	int h = (int)(s->GetTexture()->GetScale().y * th);
	if (is_8bits)
	{
		// Unscale 8 bits textures, quality won't be nice but format is really awful
		w = tw;
		h = th;
	}

	const GSVector2i dstsize = s->GetTexture()->GetSize();
	GSVector2 scale = s->GetTexture()->GetScale();
	GSVector4 dRect(p_rect_s.x, p_rect_s.y, p_rect_s.z + w, p_rect_s.w + h);

	// Lengthy explanation of the rescaling code.
	// Here an example in 2x:
	// RT is 1280x1024 but only contains 512x448 valid data (so 256x224 pixels without upscaling)
	//
	// PS2 want to read it back as a 1024x1024 pixels (they don't care about the extra pixels)
	// So in theory we need to shrink a 2048x2048 RT into a 1024x1024 texture. Obviously the RT is
	// too small.
	//
	// So we will only limit the resize to the available data in RT.
	// Therefore we will resize the RT from 1280x1024 to 1280x1024/2048x2048 % of the new texture
	// size (which is 1280x1024) (i.e. 800x512)
	// From the rendering point of view. UV coordinate will be normalized on the real GS texture size
	// This way it can be used on an upscaled texture without extra scaling factor (only requirement is
	// to have same proportion)
	//
	// FIXME: The scaling will create a bad offset. For example if texture coordinate start at 0.5 (pixel 0)
	// At 2x it will become 0.5/128 * 256 = 1 (pixel 1)
	// I think it is the purpose of the UserHacks_HalfPixelOffset below. However implementation is less
	// than ideal.
	// 1/ It suppose games have an half pixel offset on texture coordinate which could be wrong
	// 2/ It doesn't support rescaling of the RT (tw = 1024)
	// Maybe it will be more easy to just round the UV value in the Vertex Shader

	if (!is_8bits)
	{
		// 8 bits handling is special due to unscaling. It is better to not execute this code
		if (w > dstsize.x)
		{
			scale.x = (float)dstsize.x / tw;
			dRect.z = (float)dstsize.x * scale.x / s->GetTexture()->GetScale().x;
			w = dstsize.x;
		}

		if (h > dstsize.y)
		{
			scale.y = (float)dstsize.y / th;
			dRect.w = (float)dstsize.y * scale.y / s->GetTexture()->GetScale().y;
			h = dstsize.y;
		}
	}

	GSVector4 sRect(p_rect_t.x, p_rect_t.y, p_rect_t.z + w, p_rect_t.w + h);

	// Disable linear filtering for various GS post-processing effect
	// 1/ Palette is used to interpret the alpha channel of the RT as an index.
	// Star Ocean 3 uses it to emulate a stencil buffer.
	// 2/ Z formats are a bad idea to interpolate (discontinuties).
	// 3/ 16 bits buffer is used to move data from a channel to another.
	//
	// I keep linear filtering for standard color even if I'm not sure that it is
	// working correctly.
	// Indeed, texture is reduced so you need to read all covered pixels (9 in 3x)
	// to correctly interpolate the value. Linear interpolation is likely acceptable
	// only in 2x scaling
	//
	// Src texture will still be bilinear interpolated so I'm really not sure
	// that we need to do it here too.
	//
	// Future note: instead to do
	// RT 2048x2048 -> T 1024x1024 -> RT 2048x2048
	// We can maybe sample directly a bigger texture
	// RT 2048x2048 -> T 2048x2048 -> RT 2048x2048
	// Pro: better quality. Copy instead of StretchRect (must be faster)
	// Cons: consume more memory
	//
	// In distant future: investigate to reuse the RT directly without any
	// copy. Likely a speed boost and memory usage reduction.

	if ((sRect == dRect).alltrue() && shader == ShaderConvert::COPY)
	{
		// TODO Check sRect.
		m_renderer->m_dev->CopyRect(sTex, dTex, GSVector4i(sRect));
	}
	else
	{
		// Different size or not the same format
		sRect /= size_t;

		m_renderer->m_dev->StretchRect(sTex, sRect, dTex, dRect, shader, linear);
	}

	if (s->GetTexture())
		s->GetTexture()->SetScale(scale);

	ASSERT(s->GetTexture());

	/*
	// Offset hack. Can be enabled via GSdx options.
	// The offset will be used in Draw().

	float modx = 0.0f;
	float mody = 0.0f;

	if (UserHacks_HalfPixelOffset && hack)
	{
		switch (m_renderer->GetUpscaleMultiplier())
		{
		case 0: //Custom Resolution
		{
			const float offset = 0.2f;
			modx = dst->GetTexture()->GetScale().x + offset;
			mody = dst->GetTexture()->GetScale().y + offset;
			dst->GetTexture()->LikelyOffset = true;
			break;
		}
		case 2:  modx = 2.2f; mody = 2.2f; dst->GetTexture()->LikelyOffset = true;  break;
		case 3:  modx = 3.1f; mody = 3.1f; dst->GetTexture()->LikelyOffset = true;  break;
		case 4:  modx = 4.2f; mody = 4.2f; dst->GetTexture()->LikelyOffset = true;  break;
		case 5:  modx = 5.3f; mody = 5.3f; dst->GetTexture()->LikelyOffset = true;  break;
		case 6:  modx = 6.2f; mody = 6.2f; dst->GetTexture()->LikelyOffset = true;  break;
		case 8:  modx = 8.2f; mody = 8.2f; dst->GetTexture()->LikelyOffset = true;  break;
		default: modx = 0.0f; mody = 0.0f; dst->GetTexture()->LikelyOffset = false; break;
		}
	}

	dst->GetTexture()->OffsetHack_modx = modx;
	dst->GetTexture()->OffsetHack_mody = mody;
	*/

	// TODO Restore offset hack.
}

void GSTextureCache::UpdateSurface(Surface* s, const GSVector4i& rect, const int layer)
{
	assert(m_surfaces.find(s) != m_surfaces.cend());
	ASSERT(rect.rintersect(s->m_rect).eq(rect));
	ASSERT(s->m_type == SurfaceType::Source || layer == 0);
	
	assert(layer == 0);
	
	bool is_complete = true;

#ifdef _DEBUG
	std::array<u32, MAX_PAGES> pages;
	std::array<u32, MAX_PAGES> pages2;
	pages.fill(0);
	pages2.fill(0);
#endif
	s->m_off.pageLooperForRect(rect).loopPages([&](const u32 p) {
#ifdef _DEBUG
		assert(!pages.at(p));
		pages.at(p) = 999;
#endif
		if (!s->IsDirtyPage(p))
			return;
		else
			is_complete = false;
		PageInfo& pi = m_pages.at(p);
		if (pi.fb)
		{
			assert(!pi.fb->IsDirtyPage(p));
			assert(pi.fb != s);
			bool result = false;
			UpdateSurfacePage(s, pi.fb, p, result);
			if (result)
				s->SetPageValidity(p, std::numeric_limits<u32>::max());
			else if (!pi.is_sync())
			{
				GL_INS("Page readback due to UNSUPPORTED UPDATE: #%u, %s", p, pi.to_string().c_str());
				// TODO Detect and record page column overflow.
				const GSVector4i fb_p_rect = pi.fb->m_off.GetRect(p);
				Read(pi.fb, fb_p_rect);
				pi.state = PageState::CPU;
				assert(pi.is_sync());
			}
		}
		else
			assert(pi.state == PageState::CPU);
#ifdef _DEBUG
		pages.at(p) = s->IsDirtyPage(p) ? 2 : 1; // 1 if the page is clean, 2 if the page is dirty.
#endif
		// TODO Loop repeating pages.
		assert(!s->IsDirtyPage(p) || pi.state == PageState::CPU);
		// TODO Copy from other copies.
	});

	if (is_complete)
		return;

	const GSLocalMemory::psm_t& psm_t = GSLocalMemory::m_psm[s->m_TEX0.PSM];
	const GSVector2i& bs = psm_t.bs;
	const GSVector2i& pgs = psm_t.pgs;
	// Render targets and depth stencils are page aligned.
	const GSVector4i r = rect.ralign<Align_Outside>(s->m_type == SurfaceType::Source ? bs : pgs);
	const GSOffset& off = s->m_off;
	GSOffset::BNHelper bn = off.bnMulti(r.left, r.top);
	const bool repeating = s->m_repeating;
	u32 blocks = 0;

	for (int y = r.top; y < r.bottom; y += bs.y, bn.nextBlockY())
	{
		for (int x = r.left; x < r.right; x += bs.x, bn.nextBlockX())
		{
			const u32 block = repeating ? ((bn.blkY() << 7) + bn.blkX()) % MAX_BLOCKS : bn.value();
			const u32 row = block >> 5u;
#ifdef _DEBUG
			pages2.at(row) = 1;
#endif
			const u32 col = 1 << (block & 31u);
			assert(col);
			const u32 v0 = s->GetPageValidity(row);
			if ((v0 & col) == 0)
			{
				assert(s->IsDirtyPage(row));
				assert(m_pages.at(row).state == PageState::CPU);
				assert(pages.at(row) >= 2); // Page needs clean.
#ifdef _DEBUG
				++pages.at(row); // Cleaned page.
#endif
				const u32 v = v0 | col;
				assert(v);
				s->SetPageValidity(row, v);
				s->Write(GSVector4i(x, y, x + bs.x, y + bs.y), layer);
				blocks++;
			}
			assert(s->GetPageValidity(row));
		}
	}

	if (blocks > 0)
	{
		m_renderer->m_perfmon.Put(GSPerfMon::Unswizzle, bs.x * bs.y * blocks << (s->GetPalette() ? 2 : 0));

		s->Flush(0, layer);
	}

#ifdef _DEBUG
	for (u32 p = 0; p < MAX_PAGES; ++p)
	{
		const u32 p1 = pages.at(p);
		const u32 p2 = pages2.at(p);
		if (p1 && !p2)
		{
			Console.Error("PAGE %u not traversed by BNHelper.", p);
			assert(false);
		}
		else if (p1 == 2)
		{
			if (IsFb(s->m_type))
				assert(false);
		}
	}

	s->m_off.pageLooperForRect(rect).loopPages([&](const u32 p) {
		if (IsFb(s->m_type))
			assert(!s->IsDirtyPage(p));
		else
			assert(s->GetPageValidity(p));
	});
#endif
}

void GSTextureCache::UpdateSurfaceLayer(Surface* s, const GIFRegTEX0& TEX0, const GSVector4i& rect, const int layer)
{
	// TODO Fix true GS mipmapping.
	assert(false);

	ASSERT(TEX0.TBP0 < 0x4000);
	ASSERT(TEX0.TBW > 0);
	ASSERT(TEX0.PSM < 64);
	if (layer > 6)
		return;

	if (TEX0 == s->m_layer_TEX0[layer])
		return;

	const GIFRegTEX0 old_TEX0 = s->m_TEX0;

	s->m_layer_TEX0[layer] = TEX0;
	// s->m_TEX0 = TEX0;

	UpdateSurface(s, rect, layer);

	// s->m_TEX0 = old_TEX0;
}

void GSTextureCache::PrintMemoryUsage()
{
#ifdef ENABLE_OGL_DEBUG
	u32 tex = 0;
	u32 rt = 0;
	u32 dss = 0;
	for (auto& s : m_surfaces)
	{
		ASSERT(s);
		if (s)
		{
			const u32 mem = s->GetTexture()->GetMemUsage();
			switch (s->m_type)
			{
				case (SurfaceType::Source):
					tex += mem;
					break;
				case (SurfaceType::RenderTarget):
					rt += mem;
					break;
				case (SurfaceType::DepthStencil):
					dss += mem;
					break;
				case (SurfaceType::Invalid):
					assert(false);
					break;
			}
		}
	}
	GL_PERF("MEM: Tex %dMB. Target %dMB. Depth %dMB", tex >> 20u, rt >> 20u, dss >> 20u);
#endif
}

GSTextureCache::PaletteMap& GSTextureCache::GetPaletteMap()
{
	return m_palette_map;
}

// GSTextureCache::Surface

GSTextureCache::Surface::Surface(GSRenderer* r, u8* temp, const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, PaletteMap& pm, const bool paltex, const GSVector4i& rect, const SurfaceType type)
	: m_renderer(r)
	, m_texture(nullptr)
	, m_temp(temp)
	, m_rect(0, 0)
	, m_type(type)
	, m_palette_obj(nullptr)
	, m_TEX0(TEX0)
	, m_TEXA(TEXA)
	, m_pages_count(0)
	, m_paltex(paltex)
	// TODO Restore repeating textures.
	, m_repeating(false) // type == SurfaceType::Source && TEX0.IsRepeating())
	, m_p2t(m_repeating ? r->m_mem.GetPage2TileMap(TEX0) : nullptr)
	, m_scale(type == SurfaceType::Source ? 1 : r->GetTextureScaleFactor())
	, m_off(m_renderer->m_mem.GetOffset(TEX0.TBP0, TEX0.TBW, TEX0.PSM))
{
	ASSERT(type != SurfaceType::Invalid);
	ASSERT(TEX0.TBP0 < 0x4000);
	ASSERT(TEX0.TBW > 0);
	ASSERT(TEX0.PSM < 64);
	ASSERT(type != SurfaceType::Source || TEX0.TW > 0);
	ASSERT(type != SurfaceType::Source || TEX0.TH > 0);
	m_layer_TEX0.fill({});
	m_valid.fill(0);
	m_write_rect.fill({});
	m_write_count = 0;
	if (type == SurfaceType::Source)
	{
		const u32 tw = 1 << TEX0.TW;
		const u32 th = 1 << TEX0.TH;
		m_rect = GSVector4i(0, 0, tw, th);
		assert(tw >= (u32)rect.z);
		assert(th >= (u32)rect.w);
	}
	else
		m_rect = GSVector4i(0, 0, rect.z, rect.w).ralign<Align_Outside>(GSLocalMemory::m_psm[TEX0.PSM].pgs);
	assert(!m_rect.x);
	assert(!m_rect.y);
	m_pages_count = 0;
	m_off.pageLooperForRect(m_rect).loopPages([&](const u32 p) {
		++m_pages_count;
	});
	m_fully_dirty_pages_count = m_pages_count;
	const u32 w = m_rect.z * m_scale;
	const u32 h = m_rect.w * m_scale;
	if (type == SurfaceType::Source)
	{
		const GSLocalMemory::psm_t& psm_s = GSLocalMemory::m_psm[TEX0.PSM];
		m_texture = m_renderer->m_dev->CreateTexture(w, h, paltex && psm_s.pal > 0 ? GSTexture::Format::UNorm8 : GSTexture::Format::Color);
		if (psm_s.pal > 0)
			AttachPalette(pm);
	}
	else
	{
		m_texture = type == SurfaceType::RenderTarget ?
                        m_renderer->m_dev->CreateSparseRenderTarget(w, h, GSTexture::Format::Color) :
                        m_renderer->m_dev->CreateSparseDepthStencil(w, h, GSTexture::Format::DepthStencil);
	}
	assert(m_texture);
	m_texture->SetScale(m_scale);
}

GSTextureCache::Surface::~Surface()
{
	m_renderer->m_dev->Recycle(m_texture);
	m_texture = nullptr;
}

bool GSTextureCache::Surface::IsFullyDirty() const
{
#ifdef _DEBUG
	if (m_pages_count == m_fully_dirty_pages_count)
		for (const u32 v : m_valid)
			assert(!v);
#endif
	return m_pages_count == m_fully_dirty_pages_count;
}

u32 GSTextureCache::Surface::GetPageValidity(const u32 p) const
{
	assert(p < MAX_PAGES);
	return m_valid.at(p);
}

void GSTextureCache::Surface::SetPageValidity(const u32 p, const u32 v)
{
	assert(p < MAX_PAGES);
	SetSinglePageValidity(p, v); // TODO Implement partial invalidation.
#if 0
	if (m_repeating)
	{
		assert(m_p2t);
		// Note: very hot path on snowbling engine games.
		for (const GSVector2i& k : m_p2t[p])
		{
			const u32 v0 = m_valid.at(k.x);
			if (v0)
				SetSinglePageValidity(k.x, v0 | (v0 & ~k.y));
		}
	}
	else
		SetSinglePageValidity(p, v);
#endif
}

u32 GSTextureCache::Surface::GetBlockPointerPage() const
{
	return m_TEX0.TBP0 >> 5;
}

void GSTextureCache::Surface::Write(const GSVector4i& r, const int layer)
{
	m_write_rect[m_write_count++] = r;

	while (m_write_count >= 2)
	{
		GSVector4i& a = m_write_rect.at(m_write_count - 2);
		const GSVector4i& b = m_write_rect.at(m_write_count - 1);

		if ((a == b.zyxw()).mask() == 0xfff0)
		{
			assert(a.right == b.left);
			assert(a.top == b.top);
			assert(a.bottom == b.bottom);
			a.right = b.right; // extend right

			m_write_count--;
		}
		else if ((a == b.xwzy()).mask() == 0xff0f)
		{
			assert(a.left == b.left);
			assert(a.bottom == b.top);
			assert(a.right == b.right);
			a.bottom = b.bottom; // extend down

			m_write_count--;
		}
		else
		{
			break;
		}
	}

	if (m_write_count > 2)
	{
		Flush(1, layer);
	}
}

void GSTextureCache::Surface::Flush(const u32 count, const int layer)
{
	// 0 count will flush everything.

	// This function as written will not work for paletted formats copied from framebuffers
	// because they are 8 or 4 bit formats on the GS and the GS local memory module reads
	// these into an 8 bit format while the D3D surfaces are 32 bit.
	// However the function is never called for these cases.  This is just for information
	// should someone wish to use this function for these cases later.
	const GSLocalMemory::psm_t& psm = GSLocalMemory::m_psm[m_TEX0.PSM];

	int tw = m_rect.z;
	int th = m_rect.w;

	GSVector4i tr(0, 0, tw, th);

	int pitch = std::max(tw, psm.bs.x) * sizeof(u32);

	GSLocalMemory& mem = m_renderer->m_mem;

	const GSOffset& off = m_off;

	GSLocalMemory::readTexture rtx = psm.rtx;

	if (GetPalette())
	{
		pitch >>= 2;
		rtx = psm.rtxP;
	}

	u8* buff = m_temp;

	const u32 _count = count ? count : m_write_count;

	for (u32 i = 0; i < _count; i++)
	{
		const GSVector4i& r = m_write_rect.at(i);

		if ((r > tr).mask() & 0xff00)
		{
			(mem.*rtx)(off, r, buff, pitch, m_TEXA);

			m_texture->Update(r.rintersect(tr), buff, pitch, layer);
		}
		else
		{
			GSTexture::GSMap m;

			if (m_texture->Map(m, &r, layer))
			{
				(mem.*rtx)(off, r, m.bits, m.pitch, m_TEXA);

				m_texture->Unmap();
			}
			else
			{
				(mem.*rtx)(off, r, buff, pitch, m_TEXA);

				m_texture->Update(r, buff, pitch, layer);
			}
		}
	}

	if (_count < m_write_count)
	{
		// Warning src and destination overlap. Memmove must be used instead of memcpy
		memmove(m_write_rect.data(), &m_write_rect.at(_count), (m_write_count - _count) * sizeof(m_write_rect[0]));
	}

	m_write_count -= _count;
}

bool GSTextureCache::Surface::IsDirtyPage(const u32 p) const
{
	return GetPageValidity(p) != std::numeric_limits<u32>::max();
}

bool GSTextureCache::Surface::ClutMatch(const PaletteKey& palette_key) const
{
	if (!m_palette_obj)
		return false;
	return PaletteKeyEqual()(palette_key, m_palette_obj->GetPaletteKey());
}

void GSTextureCache::Surface::Extend(const GSVector4i& r)
{
	if (r.z > m_rect.z || r.w > m_rect.w)
	{
		GL_INS("EXTEND!");
		assert(m_type != SurfaceType::Source);
		const GSVector4i old_rect = m_rect;
		m_rect.z = std::max<u32>(m_rect.z, r.z);
		m_rect.w = std::max<u32>(m_rect.w, r.w);
		m_rect = m_rect.ralign<Align_Outside>(GSLocalMemory::m_psm[m_TEX0.PSM].pgs);
		const u32 w = m_rect.z * m_scale;
		const u32 h = m_rect.w * m_scale;
		GSTexture* tex = m_type == SurfaceType::RenderTarget ?
                             m_renderer->m_dev->CreateSparseRenderTarget(w, h, GSTexture::Format::Color) :
                             m_renderer->m_dev->CreateSparseDepthStencil(w, h, GSTexture::Format::DepthStencil);
		m_renderer->m_dev->CopyRect(m_texture, tex, old_rect);
		m_renderer->m_dev->Recycle(m_texture);
		m_texture = tex;
		const u32 old_pages_count = m_pages_count;
		m_pages_count = 0;
		m_off.pageLooperForRect(m_rect).loopPages([&](const u32 p) {
			++m_pages_count;
		});
		assert(m_pages_count >= old_pages_count);
		m_fully_dirty_pages_count += (m_pages_count - old_pages_count);
	}
}

GSTexture* GSTextureCache::Surface::GetPalette() const
{
	return m_palette_obj && m_paltex ? m_palette_obj->GetPaletteGSTexture() : nullptr;
}

GSTexture* GSTextureCache::Surface::GetTexture() const
{
	assert(m_texture);
	return m_texture;
}

u32 GSTextureCache::Surface::GetScale() const
{
	return m_scale;
}

void GSTextureCache::Surface::AttachPalette(PaletteMap& pm)
{
	const u16 pal = GSLocalMemory::m_psm[m_TEX0.PSM].pal;
	if (!pal)
		return;
	assert(m_type == SurfaceType::Source);
	m_palette_obj = pm.LookupPalette(pal, m_paltex);
}

void GSTextureCache::Surface::SetSinglePageValidity(const u32 p, const u32 v)
{
	assert(p < MAX_PAGES);
	const u32 v0 = m_valid.at(p);
	if (v0 != v)
	{
		m_valid.at(p) = v;
		if (!v0)
		{
			assert(m_fully_dirty_pages_count > 0);
			--m_fully_dirty_pages_count;
		}
		else if (!v)
		{
			assert(m_fully_dirty_pages_count < m_pages_count);
			++m_fully_dirty_pages_count;
		}
	}
}

// GSTextureCache::Palette

GSTextureCache::Palette::Palette(const GSRenderer* renderer, const u16 pal, const bool need_gs_texture)
	: m_pal(pal)
	, m_tex_palette(nullptr)
	, m_renderer(renderer)
{
	u16 palette_size = pal * sizeof(u32);
	m_clut = (u32*)_aligned_malloc(palette_size, 64);
	memcpy(m_clut, (const u32*)m_renderer->m_mem.m_clut, palette_size);
	if (need_gs_texture)
	{
		InitializeTexture();
	}
}

GSTextureCache::Palette::~Palette()
{
	m_renderer->m_dev->Recycle(m_tex_palette);
	_aligned_free(m_clut);
}

GSTexture* GSTextureCache::Palette::GetPaletteGSTexture() const
{
	return m_tex_palette;
}

GSTextureCache::PaletteKey GSTextureCache::Palette::GetPaletteKey() const
{
	return {m_clut, m_pal};
}

void GSTextureCache::Palette::InitializeTexture()
{
	if (!m_tex_palette)
	{
		// A palette texture is always created with dimensions 256x1 (also in the case that m_pal is 16, thus a 16x1 texture
		// would be enough to store the CLUT data) because the coordinates that the shader uses for
		// sampling such texture are always normalized by 255.
		// This is because indexes are stored as normalized values of an RGBA texture (e.g. index 15 will be read as (15/255),
		// and therefore will read texel 15/255 * texture size).
		m_tex_palette = m_renderer->m_dev->CreateTexture(256, 1, GSTexture::Format::Color);
		m_tex_palette->Update(GSVector4i(0, 0, m_pal, 1), m_clut, m_pal * sizeof(m_clut[0]));
	}
}

// GSTextureCache::PaletteKeyHash

// Hashes the content of the clut.
// The hashing function is implemented by taking two things into account:
// 1) The clut can be an array of 16 or 256 u32 (depending on the pal parameter) and in order to speed up the computation of the hash
//    the array is hashed in blocks of 16 u32, so for clut of size 16 u32 the hashing is computed in one pass and for clut of 256 u32
//    it is computed in 16 passes,
// 2) The clut can contain many 0s, so as a way to increase the spread of hashing values for small changes in the input clut the hashing function
//    is using addition in combination with logical XOR operator; The addition constants are large prime numbers, which may help in achieving what intended.
std::size_t GSTextureCache::PaletteKeyHash::operator()(const PaletteKey& key) const
{
	u16 pal = key.pal;
	const u32* clut = key.clut;

	ASSERT((pal & 15) == 0);

	size_t clut_hash = 3831179159;
	for (u16 i = 0; i < pal; i += 16)
	{
		clut_hash = (clut_hash + 1488000301) ^ (clut[i] + 33644011);
		clut_hash = (clut_hash + 3831179159) ^ (clut[i + 1] + 47627467);
		clut_hash = (clut_hash + 3659574209) ^ (clut[i + 2] + 577038523);
		clut_hash = (clut_hash + 33644011) ^ (clut[i + 3] + 3491555267);

		clut_hash = (clut_hash + 777771959) ^ (clut[i + 4] + 3301075993);
		clut_hash = (clut_hash + 4019618579) ^ (clut[i + 5] + 4186992613);
		clut_hash = (clut_hash + 3465668953) ^ (clut[i + 6] + 3043435883);
		clut_hash = (clut_hash + 3494478943) ^ (clut[i + 7] + 3441897883);

		clut_hash = (clut_hash + 3432010979) ^ (clut[i + 8] + 2167922789);
		clut_hash = (clut_hash + 1570862863) ^ (clut[i + 9] + 3401920591);
		clut_hash = (clut_hash + 1002648679) ^ (clut[i + 10] + 1293530519);
		clut_hash = (clut_hash + 551381741) ^ (clut[i + 11] + 2539834039);

		clut_hash = (clut_hash + 3768974459) ^ (clut[i + 12] + 169943507);
		clut_hash = (clut_hash + 862380703) ^ (clut[i + 13] + 2906932549);
		clut_hash = (clut_hash + 3433082137) ^ (clut[i + 14] + 4234384109);
		clut_hash = (clut_hash + 2679083843) ^ (clut[i + 15] + 2719605247);
	}
	return clut_hash;
};

// GSTextureCache::PaletteKeyEqual

bool GSTextureCache::PaletteKeyEqual::operator()(const PaletteKey& lhs, const PaletteKey& rhs) const
{
	if (lhs.pal != rhs.pal)
	{
		return false;
	}

	return GSVector4i::compare64(lhs.clut, rhs.clut, lhs.pal * sizeof(lhs.clut[0]));
};

// GSTextureCache::PaletteMap

GSTextureCache::PaletteMap::PaletteMap(const GSRenderer* renderer)
	: m_renderer(renderer)
{
	for (auto& map : m_maps)
	{
		map.reserve(MAX_SIZE);
	}
}

std::shared_ptr<GSTextureCache::Palette> GSTextureCache::PaletteMap::LookupPalette(const u16 pal, const bool need_gs_texture)
{
	ASSERT(pal == 16 || pal == 256);

	// Choose which hash map search into:
	//    pal == 16  : index 0
	//    pal == 256 : index 1
	auto& map = m_maps[pal == 16 ? 0 : 1];

	const u32* clut = (const u32*)m_renderer->m_mem.m_clut;

	// Create PaletteKey for searching into map (clut is actually not copied, so do not store this key into the map)
	PaletteKey palette_key = {clut, pal};

	auto it1 = map.find(palette_key);

	if (it1 != map.end())
	{
		// Clut content match, HIT
		if (need_gs_texture && !it1->second->GetPaletteGSTexture())
		{
			// Generate GSTexture and upload clut content if needed and not done yet
			it1->second->InitializeTexture();
		}
		return it1->second;
	}

	// No palette with matching clut content, MISS

	if (map.size() > MAX_SIZE)
	{
		// If the map is too big, try to clean it by disposing and removing unused palettes, before adding the new one
		GL_INS("WARNING, %u-bit PaletteMap (Size %u): Max size %u exceeded, clearing unused palettes.", pal * sizeof(u32), map.size(), MAX_SIZE);

		u32 current_size = map.size();

		for (auto it = map.begin(); it != map.end();)
		{
			// If the palette is unused, there is only one shared pointers holding a reference to the unused Palette object,
			// and this shared pointer is the one stored in the map itself
			if (it->second.use_count() <= 1)
			{
				// Palette is unused
				it = map.erase(it); // Erase element from map
					// The palette object should now be gone as the shared pointer to the object in the map is deleted
			}
			else
			{
				++it;
			}
		}

		u32 cleared_palette_count = current_size - (u32)map.size();

		if (cleared_palette_count == 0)
		{
			GL_INS("ERROR, %u-bit PaletteMap (Size %u): Max size %u exceeded, could not clear any palette, negative performance impact.", pal * sizeof(u32), map.size(), MAX_SIZE);
		}
		else
		{
			map.reserve(MAX_SIZE); // Ensure map capacity is not modified by the clearing
			GL_INS("INFO, %u-bit PaletteMap (Size %u): Cleared %u palettes.", pal * sizeof(u32), map.size(), cleared_palette_count);
		}
	}

	std::shared_ptr<Palette> palette = std::make_shared<Palette>(m_renderer, pal, need_gs_texture);

	map.emplace(palette->GetPaletteKey(), palette);

	GL_CACHE("TC, %u-bit PaletteMap (Size %u): Added new palette.", pal * sizeof(u32), map.size());

	return palette;
}

void GSTextureCache::PaletteMap::Clear()
{
	for (auto& map : m_maps)
	{
		map.clear(); // Clear all the nodes of the map, deleting Palette objects managed by shared pointers as they should be unused elsewhere
		map.reserve(MAX_SIZE); // Ensure map capacity is not modified by the clearing
	}
}
