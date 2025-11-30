package edu.pjwstk.gamification.usecase.getuserinventoryitems;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.io.Serializable;
import java.util.UUID;

public record GetUserInventoryItemsCommand(
        UUID userId,
        String itemName,
        Integer itemSlot,
        Integer rarity,
        Integer page,
        Integer size
) implements Command, Serializable {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}