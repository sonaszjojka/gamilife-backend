package pl.gamilife.groupshop.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupCommand;
import pl.gamilife.groupshop.application.creategroupshopforgroup.CreateGroupShopForGroupUseCase;
import pl.gamilife.shared.kernel.event.GroupCreatedEvent;

@Component
@AllArgsConstructor
public class GroupShopEventHandler {

    private final CreateGroupShopForGroupUseCase groupShopForGroupUseCase;

    @EventListener
    public void handleGroupCreatedEvent(GroupCreatedEvent event) {
        groupShopForGroupUseCase.execute(new CreateGroupShopForGroupCommand(
                event.groupId(),
                event.groupName()
        ));
    }

}
