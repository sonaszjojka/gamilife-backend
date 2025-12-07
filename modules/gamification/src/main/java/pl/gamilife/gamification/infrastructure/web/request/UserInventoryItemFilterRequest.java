package pl.gamilife.gamification.infrastructure.web.request;

import java.io.Serializable;

public record UserInventoryItemFilterRequest(
        String itemName,
        Integer itemSlot,
        Integer rarity,
        Integer page,
        Integer size
) implements Serializable {
}
