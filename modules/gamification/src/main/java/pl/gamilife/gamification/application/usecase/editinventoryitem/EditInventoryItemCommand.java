package pl.gamilife.gamification.application.usecase.editinventoryitem;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditInventoryItemCommand(
        UUID userId,
        UUID userInventoryItemId,
        Integer numberOfSoldItems,
        Boolean isEquipped
) implements Command {
    @Override
    public void validate() {
        if (userId == null || userInventoryItemId == null) {
            throw new ValidationException("User ID and User Inventory ID cannot be null");
        }

        if (numberOfSoldItems == null && isEquipped == null) {
            throw new ValidationException("Either numberOfSoldItems or newIsEquipped or both must be set");
        }

        if (numberOfSoldItems != null && numberOfSoldItems <= 0) {
            throw new ValidationException("numberOfSoldItems must be a positive number");
        }
    }
}
