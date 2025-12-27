package pl.gamilife.groupshop.application.createownedgroupitem;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;


public record CreateOwnedGroupItemCommand(

        @NotNull
        UUID groupItemId,

        @NotNull
        UUID groupId,

        @NotNull
        UUID memberId,
        @NotNull
        UUID currentUserId

) implements Command {
}
