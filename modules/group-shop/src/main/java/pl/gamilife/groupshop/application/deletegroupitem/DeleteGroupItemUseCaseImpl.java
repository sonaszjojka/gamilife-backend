package pl.gamilife.groupshop.application.deletegroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

@AllArgsConstructor
@Service
public class DeleteGroupItemUseCaseImpl implements DeleteGroupItemUseCase {

    private final GroupContext groupContext;
    private final GroupItemRepository groupItemRepository;
    private final GroupShopRepository groupShopRepository;

    @Transactional
    @Override
    public Void execute(DeleteGroupItemCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.currentUserId(), cmd.groupId()).orElseThrow(
                () -> new GroupMemberNotFoundException("User is not a member of the group")
        );
        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }


        if (!member.isAdmin()) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group item in shop!");
        }
        groupItemRepository.findById(cmd.groupItemId()).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + cmd.groupItemId() + " not found!"));

        groupItemRepository.deleteById(cmd.groupItemId());

        return null;
    }
}
