package pl.gamilife.groupshop.application.editownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.OwnedGroupItemNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.event.GroupItemUsedEvent;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@AllArgsConstructor
public class EditOwnedGroupItemUseCaseImpl implements EditOwnedGroupItemUseCase {

    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupMemberProvider;
    private final ApplicationEventPublisher eventPublisher;
    private final GroupContext groupContext;

    @Transactional
    @Override
    public EditOwnedGroupItemResult execute(EditOwnedGroupItemCommand cmd) {
        var member = groupMemberProvider.findMemberById(cmd.groupId(), cmd.groupMemberId());
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (Boolean.FALSE.equals(member.isAdmin()) && !cmd.currentUserId().equals(member.memberId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group administrators or the member themselves can edit items in inventory!");
        }

        OwnedGroupItem ownedGroupItem = ownedGroupItemRepository.findWithGroupItemById(cmd.ownedGroupItemId()).orElseThrow(
                () -> new OwnedGroupItemNotFoundException("Owned group item not found")
        );

        if (Boolean.TRUE.equals(cmd.isUsedUp())) {
            ownedGroupItem.useItem();
            eventPublisher.publishEvent(new GroupItemUsedEvent(
                    Boolean.TRUE.equals(member.isAdmin()) ? cmd.currentUserId() : groupContext.getAdminId(cmd.groupId()),
                    cmd.currentUserId(),
                    ownedGroupItem.getGroupItem().getId(),
                    ownedGroupItem.getGroupItem().getName()
            ));
        }

        return toResult(ownedGroupItem);
    }

    private EditOwnedGroupItemResult toResult(OwnedGroupItem ownedGroupItem) {

        return new EditOwnedGroupItemResult(
                ownedGroupItem.getUsedAt()
        );
    }
}
