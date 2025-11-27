package edu.pjwstk.user.usecase.grantrewardstouser;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record GrantRewardsToUserCommand(UUID userId, int experience, int money) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (experience < 0) {
            throw new ValidationException("Experience cannot be negative");
        }
    }
}
