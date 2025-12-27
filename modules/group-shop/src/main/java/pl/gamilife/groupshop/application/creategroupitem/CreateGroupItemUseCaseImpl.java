package pl.gamilife.groupshop.application.creategroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

@AllArgsConstructor
@Component
public class CreateGroupItemUseCaseImpl implements CreateGroupItemInShopUseCase {
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupProvider;
    private final CurrentUserContext currentUserProvider;

    @Transactional
    @Override
    public CreateGroupItemInShopResult execute(CreateGroupItemInShopCommand cmd) {

        GroupShop groupShop = groupShopRepository.findByGroupShopId(cmd.groupShopId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + cmd.groupShopId() + " not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        GroupForShop groupDto = groupProvider.findGroupById(cmd.groupId());
        if (!groupShop.getGroupId().equals(cmd.groupId())) {
            throw new GroupShopNotFoundException("Group shop with id: " + cmd.groupShopId() + " does not belong to group with id: " + cmd.groupId() + "!");
        }

        GroupShopUser currentUserDto = currentUserProvider.findGroupShopUserById(cmd.userId());

        if (!currentUserDto.userId().equals(groupDto.adminId()) && Boolean.TRUE.equals(cmd.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create active group item in shop!");
        }
       GroupItem groupItem = GroupItem.createPrivate(cmd.name(), cmd.price(),cmd.isActive() ,groupShop);
        groupItemInShopRepository.save(groupItem);
        return toResult(groupItem);
    }


    private CreateGroupItemInShopResult toResult(GroupItem groupItem)
    {
        return new CreateGroupItemInShopResult(
                groupItem.getId(),
                groupItem.getName(),
                groupItem.getPrice(),
                groupItem.getIsActive(),
                groupItem.getGroupShop().getId()

        );
    }
}
