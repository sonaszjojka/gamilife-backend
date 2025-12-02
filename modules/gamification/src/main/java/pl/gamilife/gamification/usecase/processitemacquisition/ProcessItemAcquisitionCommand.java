package pl.gamilife.gamification.usecase.processitemacquisition;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record ProcessItemAcquisitionCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
