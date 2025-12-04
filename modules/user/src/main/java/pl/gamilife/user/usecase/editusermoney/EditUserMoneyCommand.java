package pl.gamilife.user.usecase.editusermoney;

import jakarta.validation.ValidationException;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record EditUserMoneyCommand(UUID userId, int money) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (money == 0) {
            throw new ValidationException("Money cannot be 0");
        }
    }
}
