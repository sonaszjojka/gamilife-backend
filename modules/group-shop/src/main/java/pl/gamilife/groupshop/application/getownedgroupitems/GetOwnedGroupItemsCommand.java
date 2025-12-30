package pl.gamilife.groupshop.application.getownedgroupitems;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetOwnedGroupItemsCommand(

        @NotNull
        UUID groupId,

        @NotNull
        UUID memberId,

        @NotNull
        UUID userId,

        Boolean isUsedUp,

        Integer page,

        Integer size

) implements Command {
}
