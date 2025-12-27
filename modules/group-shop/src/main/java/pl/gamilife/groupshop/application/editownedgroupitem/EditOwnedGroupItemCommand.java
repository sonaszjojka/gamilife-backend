package pl.gamilife.groupshop.application.editownedgroupitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditOwnedGroupItemCommand(
        @NotNull
        Boolean isUsedUp,

        @NotNull
        UUID ownedGroupItemId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID currentUserId,

        @NotNull
        UUID groupMemberId

) implements Command {
}
