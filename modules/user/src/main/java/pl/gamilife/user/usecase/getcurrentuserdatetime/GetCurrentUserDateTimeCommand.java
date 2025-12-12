package pl.gamilife.user.usecase.getcurrentuserdatetime;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetCurrentUserDateTimeCommand(@NotNull UUID userId) implements Command {
}
