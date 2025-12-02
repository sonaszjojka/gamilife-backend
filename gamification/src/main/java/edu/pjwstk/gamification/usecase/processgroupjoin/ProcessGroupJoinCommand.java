package edu.pjwstk.gamification.usecase.processgroupjoin;

import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record ProcessGroupJoinCommand(UUID userId, boolean isFirstTimeJoin) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }
    }
}
