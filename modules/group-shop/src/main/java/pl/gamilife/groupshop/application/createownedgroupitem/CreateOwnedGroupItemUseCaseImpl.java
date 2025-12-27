package pl.gamilife.groupshop.application.createownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.InvalidOwnedGroupItemDataException;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRpository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@AllArgsConstructor
@Service
public class CreateOwnedGroupItemUseCaseImpl implements CreateOwnedGroupItemUseCase {

    private final GroupItemInShopRepository groupItemInShopRepository;
    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final CurrentUserContext currentUserProvider;

    @Transactional
    @Override
    public CreateOwnedGroupItemResult execute(CreateOwnedGroupItemCommand cmd) {

        GroupDto groupDto = groupProvider.findGroupById(cmd.groupId());
        GroupShopUser currentUser = currentUserProvider.findGroupShopUserById(cmd.currentUserId());

        if (groupProvider.findGroupMemberById(cmd.memberId()) == null) {
            throw new GroupMemberNotFoundException("Group member not found in the specified group!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (!currentUser.userId().equals(groupDto.adminId()) && !currentUser.userId().equals(groupProvider.findGroupMemberById(cmd.memberId()).userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group administrators or the member themselves can add items to inventory!");
        }

        GroupItem groupItem = groupItemInShopRepository.findById(cmd.groupItemId())
                .orElseThrow(() -> new RuntimeException("Group item in shop not found"));

        if (Boolean.FALSE.equals(groupItem.getIsActive())) {
            throw new InvalidOwnedGroupItemDataException("Cannot add inactive group item to inventory!");
        }

        OwnedGroupItem ownedGroupItem = OwnedGroupItem.createPrivate(
                cmd.memberId(),
                groupItem
        );

        return toResult(ownedGroupItem);
    }

    private CreateOwnedGroupItemResult  toResult(OwnedGroupItem ownedGroupItem) {
        return new CreateOwnedGroupItemResult(
                ownedGroupItem.getId(),
                ownedGroupItem.getGroupMemberId(),
                ownedGroupItem.getGroupItem().getId(),
                ownedGroupItem.getCreatedAt()
        );
    }
}
