package pl.gamilife.groupshop.application.deletegroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.exception.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
@AllArgsConstructor
@Service
public class DeleteGroupItemUseCaseImpl implements DeleteGroupItemUseCase {

    private final CurrentUserContext currentUserProvider;
    private final GroupApi groupProvider;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;

    @Transactional
    @Override
    public void deleteById(DeleteGroupItemCommand cmd) {

        GroupShopUser currentUser = currentUserProvider.findGroupShopUserById(cmd.currentUserId());
        GroupDto groupDto = groupProvider.findGroupById(cmd.groupId());

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }


        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group item in shop!");
        }
        groupItemInShopRepository.findById(cmd.groupItemId()).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + cmd.groupItemId() + " not found!"));

        groupItemInShopRepository.deleteById(cmd.groupItemId());


    }
}
