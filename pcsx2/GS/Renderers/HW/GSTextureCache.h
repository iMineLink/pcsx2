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

#pragma once

#include "GS/Renderers/Common/GSRenderer.h"
#include "GS/Renderers/Common/GSFastList.h"
#include "GS/Renderers/Common/GSDirtyRect.h"
#include <unordered_set>
#include <iomanip>
#include <limits>
#include <string>
#include <sstream>

class GSTextureCache
{
public:
	enum struct SurfaceType : u8
	{
		Invalid = 0,
		Source = 1,
		RenderTarget = 2,
		DepthStencil = 3
	};

	inline constexpr static bool IsFb(const SurfaceType t) noexcept
	{
		return t == SurfaceType::RenderTarget || t == SurfaceType::DepthStencil;
	}

	struct PaletteKey
	{
		const u32* clut;
		u16 pal;
	};

	class Palette
	{
	private:
		u32* m_clut;
		u16 m_pal;
		GSTexture* m_tex_palette;
		const GSRenderer* m_renderer;

	public:
		Palette(const GSRenderer* renderer, const u16 pal, const bool need_gs_texture);
		~Palette();

		// Disable copy constructor and copy operator
		Palette(const Palette&) = delete;
		Palette& operator=(const Palette&) = delete;

		// Disable move constructor and move operator
		Palette(const Palette&&) = delete;
		Palette& operator=(const Palette&&) = delete;

		GSTexture* GetPaletteGSTexture() const;
		PaletteKey GetPaletteKey() const;
		void InitializeTexture();
	};

	struct PaletteKeyHash
	{
		// Calculate hash
		std::size_t operator()(const PaletteKey& key) const;
	};

	struct PaletteKeyEqual
	{
		// Compare pal value and clut contents
		bool operator()(const PaletteKey& lhs, const PaletteKey& rhs) const;
	};

	class PaletteMap
	{
	private:
		static const u16 MAX_SIZE = 65535; // Max size of each map.
		const GSRenderer* m_renderer;

		// Array of 2 maps, the first for 64B palettes and the second for 1024B palettes.
		// Each map stores the key PaletteKey (clut copy, pal value) pointing to the relevant shared pointer to Palette object.
		// There is one PaletteKey per Palette, and the hashing and comparison of PaletteKey is done with custom operators PaletteKeyHash and PaletteKeyEqual.
		std::array<std::unordered_map<PaletteKey, std::shared_ptr<Palette>, PaletteKeyHash, PaletteKeyEqual>, 2> m_maps;

	public:
		PaletteMap(const GSRenderer* renderer);

		// Retrieves a shared pointer to a valid Palette from m_maps or creates a new one adding it to the data structure
		std::shared_ptr<Palette> LookupPalette(const u16 pal, const bool need_gs_texture);

		void Clear(); // Clears m_maps, thus deletes Palette objects
	};

	class Surface : public GSAlignedClass<32>
	{
	protected:
		GSRenderer* m_renderer;

	public:
		const GIFRegTEX0 m_TEX0;
		const GIFRegTEXA m_TEXA;
		u8* m_temp;
		const GSOffset m_off;
		GSVector4i m_rect; // The surface rect.
		const bool m_repeating;
		const std::vector<GSVector2i>* m_p2t;
		const SurfaceType m_type;
		std::array<GIFRegTEX0, 7> m_layer_TEX0; // Detect already loaded value

		Surface(GSRenderer* r, u8* temp, const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, PaletteMap& pm, const bool paltex, const GSVector4i& rect, const SurfaceType type);
		~Surface();

		bool IsFullyDirty() const;
		u32 GetPageValidity(const u32 p) const;
		void SetPageValidity(const u32 p, const u32 v);
		u32 GetBlockPointerPage() const;
		void Write(const GSVector4i& r, const int layer);
		void Flush(const u32 count, const int layer);
		bool IsDirtyPage(const u32 p) const;
		bool ClutMatch(const PaletteKey& palette_key) const;
		void Extend(const GSVector4i& r);
		GSTexture* GetPalette() const;
		GSTexture* GetTexture() const;
		u32 GetScale() const;
		void AttachPalette(PaletteMap& pm);

	private:
		GSTexture* m_texture;
		size_t m_fully_dirty_pages_count;
		size_t m_pages_count;
		std::array<u32, MAX_PAGES> m_valid; // each u32 bits map to the 32 blocks of that page
		std::shared_ptr<Palette> m_palette_obj;
		const u32 m_scale;
		const bool m_paltex;
		std::array<GSVector4i, 3> m_write_rect;
		u32 m_write_count;

		void SetSinglePageValidity(const u32 p, const u32 v);
	};

	enum struct PageState : u8
	{
		CPU = 0,
		GPU = 1,
	};

	struct PageInfo
	{
		PageState state;
		FastList<Surface*> copies; // Page may be not fully valid.
		Surface* fb; // Page fully valid.

		PageInfo() noexcept
			: state(PageState::CPU)
			, fb(nullptr)
		{
		}

		bool is_sync() const noexcept
		{
			assert(!fb || IsFb(fb->m_type));
			return state == PageState::CPU && fb;
		}

		std::string to_string() const
		{
			std::stringstream ss;
			ss << "State: " << (state == PageState::CPU ? "CPU" : "GPU") << ", ";
			ss << "Sync: " << (is_sync() ? "YES" : "NO") << ", ";
			ss << "Copies: " << copies.size();
			if (fb)
			{
				ss << ", ";
				ss << "FbAddr: 0x" << std::hex << std::setw(8) << std::setfill('0') << fb << std::dec << ", ";
				ss << "FbTexID: " << fb->GetTexture()->GetID();
			}
			return ss.str();
		}
	};

protected:
	GSRenderer* m_renderer;
	PaletteMap m_palette_map;
	std::unordered_set<Surface*> m_surfaces;
	bool m_paltex;
	u8* m_temp;
	CRCHackLevel m_crc_hack_level;
	std::array<PageInfo, MAX_PAGES> m_pages;

public:
	GSTextureCache(GSRenderer* r);
	~GSTextureCache();
	void Read(Surface* t, const GSVector4i& r);
	void RemoveAll();

	Surface* LookupSource(const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, const GSVector4i& r);
	Surface* LookupTarget(const GIFRegTEX0& TEX0, const GSVector4i& r, const SurfaceType type);

	void InvalidateVideoMem(const GSOffset& off, const GSVector4i& r, Surface* fb);
	void InvalidateLocalMem(const GSOffset& off, const GSVector4i& r);

	void UpdateSurfacePage(Surface* s, const Surface* t, const u32 p, bool& out_result); // t is the source of the copy.
	void UpdateSurface(Surface* s, const GSVector4i& rect, const int layer = 0);
	void UpdateSurfaceLayer(Surface* s, const GIFRegTEX0& TEX0, const GSVector4i& rect, const int layer = 0);

	bool UserHacks_HalfPixelOffset;

	const char* to_string(const SurfaceType type) noexcept
	{
		return (type == SurfaceType::DepthStencil) ? "Depth" : "Color";
	}

	void PrintMemoryUsage();

	PaletteMap& GetPaletteMap();
};
