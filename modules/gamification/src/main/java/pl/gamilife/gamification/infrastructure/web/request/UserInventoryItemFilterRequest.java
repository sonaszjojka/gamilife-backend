package edu.pjwstk.gamification.controller.request;

import java.io.Serializable;

public record UserInventoryItemFilterRequest(
        String itemName,
        Integer itemSlot,
        Integer rarity,
        Integer page,
        Integer size
) implements Serializable {
}
