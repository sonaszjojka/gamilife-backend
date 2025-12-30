package pl.gamilife.group.usecase.editgroup;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        UUID adminId,
        String groupName,
        Character groupCurrencySymbol,
        Integer groupTypeId,
        Integer membersLimit
) implements Command {
}
