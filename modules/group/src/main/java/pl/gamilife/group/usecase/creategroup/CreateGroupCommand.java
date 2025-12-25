package pl.gamilife.group.usecase.creategroup;

import jakarta.validation.constraints.*;
import pl.gamilife.shared.kernel.architecture.Command;

public record CreateGroupCommand(
        @NotBlank
        @Size(max = 50)
        String groupName,

        @NotNull
        Character groupCurrencySymbol,

        @NotNull
        Integer groupTypeId,

        @NotNull()
        @Min(value = 2)
        @Max(value = 100)
        Integer membersLimit
) implements Command {
}
