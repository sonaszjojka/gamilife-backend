package pl.gamilife.groupshop.application.changegroupshopstatus;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;


@Service
@AllArgsConstructor
@Transactional
public class ChangeGroupShopStatusUseCaseImpl implements ChangeGroupShopStatusUseCase {
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;
    private final CurrentUserContext currentUserContext;


    @Override
    public ChangeGroupShopStatusResult execute(ChangeGroupStatusCommand cmd) {

        GroupShopUser currentUser = currentUserContext.findGroupShopUserById(cmd.userId());
        GroupForShop group = groupContext.findGroupById(cmd.groupId());
        if (!currentUser.userId().equals(group.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop for group with id: " + cmd.groupId() + " not found!"));

        groupShop.setIsActive(cmd.isActive());
        groupShopRepository.save(groupShop);

        return buildResponse(groupShop);
    }

    ChangeGroupShopStatusResult buildResponse(GroupShop groupShop) {
        return new ChangeGroupShopStatusResult(
                groupShop.getId(),
                groupShop.getGroupId(),
                groupShop.getName(),
                groupShop.getDescription(),
                groupShop.getIsActive()
        );


    }
}
