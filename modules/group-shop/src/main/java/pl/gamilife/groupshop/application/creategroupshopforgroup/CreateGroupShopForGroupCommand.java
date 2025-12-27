package pl.gamilife.groupshop.application.creategroupshopforgroup;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record CreateGroupShopForGroupCommand(
        String name,
        String description,
        UUID groupId

) implements Command {
}
