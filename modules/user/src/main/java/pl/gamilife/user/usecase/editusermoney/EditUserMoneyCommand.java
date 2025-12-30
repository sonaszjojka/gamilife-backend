package pl.gamilife.user.usecase.editusermoney;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditUserMoneyCommand(
        @NotNull
        UUID userId,

        int money
) implements Command {
}
