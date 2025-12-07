package pl.gamilife.gamification.application.usecase.processgroupjoin;

import jakarta.validation.ValidationException;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record ProcessGroupJoinCommand(UUID userId, boolean isFirstTimeJoin) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
