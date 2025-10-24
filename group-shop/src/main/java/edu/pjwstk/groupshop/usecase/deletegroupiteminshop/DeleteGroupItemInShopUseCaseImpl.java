package edu.pjwstk.groupshop.usecase.deletegroupiteminshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteGroupItemInShopUseCaseImpl implements DeleteGroupItemInShopUseCase {

    private final AuthApi currentUserProvider;
    private final GroupApi groupProvider;
    private final GroupItemInShopRepository groupItemInShopRepository;

    public DeleteGroupItemInShopUseCaseImpl(AuthApi currentUserProvider, GroupApi groupProvider, GroupItemInShopRepository groupItemInShopRepository) {
        this.currentUserProvider = currentUserProvider;
        this.groupProvider = groupProvider;
        this.groupItemInShopRepository = groupItemInShopRepository;
    }


    @Override
    public void deleteById(UUID groupItemInShopId,UUID groupId) {

        CurrentUserDto currentUser = currentUserProvider.getCurrentUser().orElseThrow();
        GroupDto groupDto = groupProvider.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new RuntimeException("Only group administrators can delete group item in shop!");
        }
        groupItemInShopRepository.deleteById(groupItemInShopId);


    }
}
