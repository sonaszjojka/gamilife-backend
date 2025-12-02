package edu.pjwstk.communication.usecase.senduseremail;

import edu.pjwstk.communication.dto.EmailParameters;
import pl.gamilife.infrastructure.core.architecture.Command;
import jakarta.validation.ValidationException;

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
