package pl.gamilife.groupshop.application.createownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.InvalidOwnedGroupItemDataException;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.projection.GroupShopMember;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@AllArgsConstructor
@Service
public class CreateOwnedGroupItemUseCaseImpl implements CreateOwnedGroupItemUseCase {

    private final GroupItemRepository groupItemRepository;
    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupMemberProvider;

    @Transactional
    @Override
    public CreateOwnedGroupItemResult execute(CreateOwnedGroupItemCommand cmd) {
        GroupShopMember member = groupMemberProvider.findMemberById(cmd.groupId(), cmd.memberId());
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (!member.isAdmin() && !cmd.currentUserId().equals(member.memberId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group administrators or the member themselves can add items to inventory!");
        }

        GroupItem groupItem = groupItemRepository.findById(cmd.groupItemId())
                .orElseThrow(() -> new RuntimeException("Group item in shop not found"));

        if (Boolean.FALSE.equals(groupItem.getIsActive())) {
            throw new InvalidOwnedGroupItemDataException("Cannot add inactive group item to inventory!");
        }

        OwnedGroupItem ownedGroupItem = OwnedGroupItem.createPrivate(
                cmd.memberId(),
                groupItem
        );

        return toResult(ownedGroupItemRepository.save(ownedGroupItem));
    }

    private CreateOwnedGroupItemResult toResult(OwnedGroupItem ownedGroupItem) {
        return new CreateOwnedGroupItemResult(
                ownedGroupItem.getId(),
                ownedGroupItem.getGroupMemberId(),
                ownedGroupItem.getGroupItem().getId(),
                ownedGroupItem.getCreatedAt()
        );
    }
}
