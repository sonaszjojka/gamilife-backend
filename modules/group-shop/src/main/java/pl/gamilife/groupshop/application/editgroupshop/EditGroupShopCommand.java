package pl.gamilife.groupshop.application.editgroupshop;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record EditGroupShopCommand(
        String name,
        String description,
        UUID groupShopId,
        UUID groupId,
        UUID userId

) implements Command {
}
