package pl.gamilife.gamification.domain.model.filter;

import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;

import java.util.UUID;

public record UserInventoryItemFilter(
        UUID userId,
        String itemName,
        ItemSlotEnum itemSlot,
        RarityEnum rarity
) {
}
