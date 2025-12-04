package pl.gamilife.gamification.usecase.editinventoryitem;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record EditInventoryItemCommand(
        UUID userId,
        UUID userInventoryItemId,
        Integer subtractQuantityBy,
        Boolean isEquipped
) implements Command {
    @Override
    public void validate() {
        if (userId == null || userInventoryItemId == null) {
            throw new ValidationException("User ID and User Inventory ID cannot be null");
        }

        if (subtractQuantityBy == null && isEquipped == null) {
            throw new ValidationException("Either subtractQuantityBy or newIsEquipped or both must be set");
        }

        if (subtractQuantityBy != null && subtractQuantityBy <= 0) {
            throw new ValidationException("SubtractQuantityBy must be a positive number");
        }
    }
}
