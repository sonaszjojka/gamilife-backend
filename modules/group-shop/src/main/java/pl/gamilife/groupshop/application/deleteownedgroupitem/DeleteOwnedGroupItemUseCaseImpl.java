package pl.gamilife.groupshop.application.deleteownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.OwnedGroupItemNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@AllArgsConstructor
public class DeleteOwnedGroupItemUseCaseImpl implements DeleteOwnedGroupItemUseCase {

    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;

    @Transactional
    @Override
    public Void execute(DeleteOwnedGroupItemCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new ResourceOwnerPrivilegesRequiredException("User is not a member of the group")
        );

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        ownedGroupItemRepository.findById(cmd.ownedGroupItemId()).orElseThrow(
                () -> new OwnedGroupItemNotFoundException("Owned group item not found"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (Boolean.FALSE.equals(member.isAdmin())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete items from inventory!");
        }

        ownedGroupItemRepository.deleteById(cmd.ownedGroupItemId());

        return null;
    }
}
