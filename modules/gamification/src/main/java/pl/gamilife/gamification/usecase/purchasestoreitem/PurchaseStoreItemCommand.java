package pl.gamilife.gamification.usecase.purchasestoreitem;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record PurchaseStoreItemCommand(
        UUID userId,
        UUID itemId
) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("UserId cannot be null");
        }

        if (itemId == null) {
            throw new ValidationException("ItemId cannot be null");
        }
    }
}
