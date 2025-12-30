package pl.gamilife.group.usecase.grantrewardstomembers;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record GrantRewardsToMembersCommand(
        Collection<UUID> groupMemberIds,
        Integer amount
) implements Command {
}
