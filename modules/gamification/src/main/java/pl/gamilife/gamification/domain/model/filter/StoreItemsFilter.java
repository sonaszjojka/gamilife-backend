package pl.gamilife.gamification.domain.model.filter;

import java.util.List;

public record StoreItemsFilter(
        String itemName,
        List<Integer> itemSlotIds,
        List<Integer> rarityIds
) {
}
