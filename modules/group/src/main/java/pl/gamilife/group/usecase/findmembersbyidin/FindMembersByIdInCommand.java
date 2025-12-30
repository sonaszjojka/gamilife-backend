package pl.gamilife.group.usecase.findmembersbyidin;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record FindMembersByIdInCommand(
        Collection<UUID> groupMemberIds
) implements Command {
}
