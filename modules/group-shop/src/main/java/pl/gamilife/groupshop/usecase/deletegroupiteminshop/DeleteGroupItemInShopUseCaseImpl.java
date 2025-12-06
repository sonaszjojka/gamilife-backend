package pl.gamilife.groupshop.usecase.deletegroupiteminshop;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.exception.domain.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.exception.domain.GroupShopNotFoundException;
import pl.gamilife.groupshop.exception.domain.InactiveGroupShopException;
import pl.gamilife.groupshop.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import pl.gamilife.infrastructure.core.exception.domain.GroupAdminPrivilegesRequiredException;

import java.util.UUID;

@Service
public class DeleteGroupItemInShopUseCaseImpl implements DeleteGroupItemInShopUseCase {

    private final AuthApi currentUserProvider;
    private final GroupApi groupProvider;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;

    public DeleteGroupItemInShopUseCaseImpl(AuthApi currentUserProvider, GroupApi groupProvider, GroupItemInShopRepository groupItemInShopRepository, GroupShopRepository groupShopRepository) {
        this.currentUserProvider = currentUserProvider;
        this.groupProvider = groupProvider;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.groupShopRepository = groupShopRepository;
    }


    @Override
    public void deleteById(UUID groupItemInShopId, UUID groupId) {

        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        GroupDto groupDto = groupProvider.findGroupById(groupId);

        GroupShop groupShop = groupShopRepository.findByGroupId(groupId).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }


        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group item in shop!");
        }
        groupItemInShopRepository.findById(groupItemInShopId).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + groupItemInShopId + " not found!"));

        groupItemInShopRepository.deleteById(groupItemInShopId);


    }
}
