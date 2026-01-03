package pl.gamilife.groupshop.application.editgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;


@Service
@AllArgsConstructor
public class EditGroupItemUseCaseImpl implements EditGroupItemUseCase {

    private final GroupItemRepository groupItemRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;


    @Transactional
    @Override
    public EditGroupItemResult execute(EditGroupItemCommand cmd) {
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        var member = groupContext.findMemberByUserId(cmd.currentUserId(), cmd.groupId()).orElseThrow(
                () -> new GroupMemberNotFoundException("User is not a member of the group")
        );

        if (!member.isAdmin() && Boolean.TRUE.equals(cmd.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can make group items active!");
        }
        GroupItem groupItem = groupItemRepository.findById(cmd.groupItemId()).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + cmd.groupItemId() + " not found!"));

        groupItem.setPrice(cmd.price());
        groupItem.setName(cmd.name());
        groupItem.setIsActive(cmd.isActive());

        groupItemRepository.save(groupItem);
        return toResult(groupItem);

    }

    private EditGroupItemResult toResult(GroupItem groupItem) {

        return new EditGroupItemResult(
                groupItem.getId(),
                groupItem.getName(),
                groupItem.getPrice(),
                groupItem.getIsActive(),
                groupItem.getGroupShopId()
        );
    }
}
