package pl.gamilife.gamification.usecase.getallitemslots;

import java.util.List;

public record GetAllItemSlotsResult(
        List<ItemSlotDTO> itemSlots
) {
    record ItemSlotDTO(
            Integer id,
            String name
    ) {
    }
}
