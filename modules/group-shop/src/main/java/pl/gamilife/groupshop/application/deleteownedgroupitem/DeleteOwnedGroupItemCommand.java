package pl.gamilife.groupshop.application.deleteownedgroupitem;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteOwnedGroupItemCommand(

        UUID groupId,
        UUID memberId,
        UUID ownedGroupItemId,
        UUID currentUserId
) implements Command {
}
