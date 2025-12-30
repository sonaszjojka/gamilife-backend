package pl.gamilife.user.usecase.grantrewardstouser;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GrantRewardsToUserCommand(
        @NotNull
        UUID userId,

        @PositiveOrZero
        int experience,

        @PositiveOrZero
        int money
) implements Command {
}
