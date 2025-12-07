package pl.gamilife.gamification.application.usecase.getuserinventoryitems;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

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