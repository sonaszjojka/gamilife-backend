package pl.gamilife.gamification.domain.model.filter;

import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;

public record StoreItemsFilter(
        String itemName,
        ItemSlotEnum [] itemSlot,
        RarityEnum [] rarity
) {
}
