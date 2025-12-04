package pl.gamilife.communication.usecase.senduseremail;

import jakarta.validation.ValidationException;
import pl.gamilife.communication.dto.EmailParameters;
import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record SendUserEmailCommand(
        UUID userId,
        EmailParameters emailParameters
) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new ValidationException("User id cannot be null");
        }

        if (emailParameters == null) {
            throw new ValidationException("Email parameters cannot be null");
        }
    }
}
