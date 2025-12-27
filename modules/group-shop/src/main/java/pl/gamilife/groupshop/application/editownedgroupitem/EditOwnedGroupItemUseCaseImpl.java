package pl.gamilife.groupshop.application.editownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.domain.exception.OwnedGroupItemNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRpository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@AllArgsConstructor
public class EditOwnedGroupItemUseCaseImpl implements EditOwnedGroupItemUseCase {

    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final GroupShopRepository groupShopRepository;
    private final CurrentUserContext currentUserProvider;
    private final GroupApi groupProvider;

    @Transactional
    @Override
    public EditOwnedGroupItemResult execute(EditOwnedGroupItemCommand cmd) {

        if (groupProvider.findGroupMemberById(cmd.groupMemberId()) == null) {
            throw new GroupMemberNotFoundException("Group member not found in the specified group!");
        }
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        GroupShopUser currentUserDto = currentUserProvider.findGroupShopUserById(cmd.currentUserId());
        GroupDto groupDto = groupProvider.findGroupById(cmd.groupId());
        if (!currentUserDto.userId().equals(groupDto.adminId()) && !currentUserDto.userId().equals(groupProvider.findGroupMemberById(cmd.groupMemberId()).userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group administrators or the member themselves can edit items in inventory!");
        }

        OwnedGroupItem ownedGroupItem = ownedGroupItemRpository.findById(cmd.ownedGroupItemId()).orElseThrow(
                () -> new OwnedGroupItemNotFoundException("Owned group item not found")
        );


        ownedGroupItem.useItem(cmd.isUsedUp());

        return toResult(ownedGroupItem);
    }

    private EditOwnedGroupItemResult toResult(OwnedGroupItem ownedGroupItem) {

        return new EditOwnedGroupItemResult(
                ownedGroupItem.getUseDate()
        );
    }
}
