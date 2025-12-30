package pl.gamilife.group.usecase.makepayment;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record MakePaymentCommand(
        @NotNull
        UUID groupMemberId,

        @NotNull
        @Positive
        Integer amount
) implements Command {
}
