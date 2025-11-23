package edu.pjwstk.groupshop.usecase.deletegroupiteminshop;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.domain.GroupShopItemNotFoundException;
import edu.pjwstk.groupshop.exception.domain.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.domain.InactiveGroupShopException;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

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
