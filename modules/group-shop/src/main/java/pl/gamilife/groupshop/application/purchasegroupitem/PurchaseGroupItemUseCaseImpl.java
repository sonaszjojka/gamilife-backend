package pl.gamilife.groupshop.application.purchasegroupitem;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
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
import pl.gamilife.shared.kernel.event.GroupItemPurchasedEvent;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@AllArgsConstructor
@Service
public class PurchaseGroupItemUseCaseImpl implements PurchaseGroupItemUseCase {

    private final GroupItemRepository groupItemRepository;
    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupMemberProvider;
    private final GroupContext groupContext;
    private final ApplicationEventPublisher eventPublisher;

    @Transactional
    @Override
    public PurchaseGroupItemResult execute(PurchaseGroupItemCommand cmd) {
        GroupShopMember member = groupMemberProvider.findMemberById(cmd.groupId(), cmd.memberId());
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (!cmd.currentUserId().equals(member.memberId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only members can purchase items for themselves!");
        }

        GroupItem groupItem = groupItemRepository.findById(cmd.groupItemId())
                .orElseThrow(() -> new RuntimeException("Group item in shop not found"));

        if (Boolean.FALSE.equals(groupItem.getIsActive())) {
            throw new InvalidOwnedGroupItemDataException("Cannot add inactive group item to inventory!");
        }

        groupContext.payForItem(cmd.memberId(), groupItem.getPrice());

        OwnedGroupItem ownedGroupItem = OwnedGroupItem.create(
                cmd.memberId(),
                groupItem
        );

        eventPublisher.publishEvent(new GroupItemPurchasedEvent(cmd.currentUserId()));

        return toResult(ownedGroupItemRepository.save(ownedGroupItem));
    }

    private PurchaseGroupItemResult toResult(OwnedGroupItem ownedGroupItem) {
        return new PurchaseGroupItemResult(
                ownedGroupItem.getId(),
                ownedGroupItem.getGroupMemberId(),
                ownedGroupItem.getGroupItem().getId(),
                ownedGroupItem.getCreatedAt()
        );
    }
}
