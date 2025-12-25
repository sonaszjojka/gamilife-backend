package pl.gamilife.communication.usecase.senduseremail;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.communication.dto.EmailParameters;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record SendUserEmailCommand(
        @NotNull
        UUID userId,

        @NotNull
        EmailParameters emailParameters
) implements Command {
}
