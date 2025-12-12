package pl.gamilife.user.usecase.updateusertimezone;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record UpdateUserTimezoneCommand(@NotNull UUID userId, @NotBlank String timezone) implements Command {
}
