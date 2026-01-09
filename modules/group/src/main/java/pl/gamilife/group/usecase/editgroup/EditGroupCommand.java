package pl.gamilife.group.usecase.editgroup;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record EditGroupCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID groupId,

        UUID adminId,
        String groupName,
        String groupCurrencySymbol,
        Integer groupTypeId,
        Integer membersLimit,
        ZoneId groupTimeZoneId
) implements Command {
}
