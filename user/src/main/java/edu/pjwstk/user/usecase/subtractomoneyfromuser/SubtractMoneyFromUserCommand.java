package edu.pjwstk.user.usecase.subtractomoneyfromuser;

import edu.pjwstk.core.Command;
import jakarta.validation.ValidationException;

import java.util.UUID;

public record SubtractMoneyFromUserCommand(UUID userId, int money) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (money < 0) {
            throw new ValidationException("Money cannot be less or equal to 0");
        }
    }
}
