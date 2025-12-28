package pl.gamilife.user.usecase.getusers;

import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

public record GetUsersCommand(
        String username,

        @PositiveOrZero
        int page,

        @Positive
        int size
) implements Command {
}
