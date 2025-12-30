package pl.gamilife.groupshop.application.creategroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

@AllArgsConstructor
@Service
public class CreateGroupItemUseCaseImpl implements CreateGroupItemInShopUseCase {

    private final GroupItemRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;

    @Transactional
    @Override
    public CreateGroupItemInShopResult execute(CreateGroupItemInShopCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new GroupMemberNotFoundException("User is not a member of the group")
        );

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop for group with id: " + cmd.groupId() + " not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (!member.isAdmin()) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create active group item in shop!");
        }

        GroupItem groupItem = GroupItem.createPrivate(cmd.name(), cmd.price(), cmd.isActive(), groupShop);
        groupItemInShopRepository.save(groupItem);
        return toResult(groupItem);
    }

    private CreateGroupItemInShopResult toResult(GroupItem groupItem) {
        return new CreateGroupItemInShopResult(
                groupItem.getId(),
                groupItem.getName(),
                groupItem.getPrice(),
                groupItem.getIsActive()
        );
    }
}
