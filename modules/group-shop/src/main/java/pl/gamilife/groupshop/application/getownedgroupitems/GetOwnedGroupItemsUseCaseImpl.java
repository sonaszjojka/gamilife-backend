package pl.gamilife.groupshop.application.getownedgroupitems;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.filter.OwnedGroupItemsFilter;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Transactional(readOnly = true)
@AllArgsConstructor
@Service
public class GetOwnedGroupItemsUseCaseImpl implements GetOwnedGroupItemsUseCase {

    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final GroupContext groupContext;

    @Override
    public Page<GetOwnedGroupItemsResult> execute(GetOwnedGroupItemsCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new ResourceOwnerPrivilegesRequiredException("User is not a member of the group")
        );

        if (!member.memberId().equals(cmd.memberId()) && !member.isAdmin()) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not a the specified member of the group");
        }

        Page<OwnedGroupItem> ownedGroupItemPage = ownedGroupItemRepository.findAllMemberItems(
                new OwnedGroupItemsFilter(cmd.memberId(), cmd.isUsedUp()),
                cmd.page(),
                cmd.size()
        );

        return ownedGroupItemPage.map(this::toResult);
    }

    private GetOwnedGroupItemsResult toResult(OwnedGroupItem ownedGroupItem) {
        return new GetOwnedGroupItemsResult(
                ownedGroupItem.getId(),
                ownedGroupItem.getGroupMemberId(),
                ownedGroupItem.getUsedAt(),
                new GetOwnedGroupItemsResult.GroupShopItemDto(
                        ownedGroupItem.getGroupItem().getId(),
                        ownedGroupItem.getGroupItem().getName(),
                        ownedGroupItem.getGroupItem().getPrice(),
                        ownedGroupItem.getGroupItem().getIsActive()
                )

        );
    }
}
