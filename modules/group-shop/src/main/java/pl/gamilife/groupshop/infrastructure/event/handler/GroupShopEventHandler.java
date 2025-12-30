package pl.gamilife.groupshop.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupCommand;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupUseCase;
import pl.gamilife.groupshop.application.deletegroupshop.DeleteGroupShopCommand;
import pl.gamilife.groupshop.application.deletegroupshop.DeleteGroupShopUseCase;
import pl.gamilife.shared.kernel.event.GroupCreatedEvent;
import pl.gamilife.shared.kernel.event.GroupDeletionRequestedEvent;

@Component
@AllArgsConstructor
public class GroupShopEventHandler {

    private final CreateGroupShopForGroupUseCase createGroupShopForGroupUseCase;
    private final DeleteGroupShopUseCase deleteGroupShopUseCase;

    @EventListener
    public void handleGroupCreatedEvent(GroupCreatedEvent event) {
        createGroupShopForGroupUseCase.execute(new CreateGroupShopForGroupCommand(
                event.groupId(),
                event.groupName()
        ));
    }

    @EventListener
    public void handleGroupDeletionRequestedEvent(GroupDeletionRequestedEvent event) {
        deleteGroupShopUseCase.execute(new DeleteGroupShopCommand(event.groupId()));
    }

}
