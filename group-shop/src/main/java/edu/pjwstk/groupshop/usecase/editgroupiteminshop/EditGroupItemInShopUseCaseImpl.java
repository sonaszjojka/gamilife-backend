package edu.pjwstk.groupshop.usecase.editgroupiteminshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.exception.GroupItemInShopNotFoundException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopMapper;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupItemInShopUseCaseImpl implements EditGroupItemInShopUseCase {
    private final EditGroupItemInShopMapper editGroupItemInShopMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final AuthApi currentUserProvider;
    private final GroupApi groupApi;
    public EditGroupItemInShopUseCaseImpl(EditGroupItemInShopMapper editGroupItemInShopMapper, GroupItemInShopRepository groupItemInShopRepository, AuthApi currentUserProvider, GroupApi groupApi) {
        this.editGroupItemInShopMapper = editGroupItemInShopMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.currentUserProvider = currentUserProvider;
        this.groupApi = groupApi;
    }

    @Override
    public EditGroupItemInShopResponse execute(UUID groupItemId, UUID groupId, EditGroupItemInShopRequest request) {

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser()
                .orElseThrow();
        GroupDto groupDto = groupApi.findGroupById(groupId);

        if (!currentUserDto.userId().equals(groupDto.adminId())&& Boolean.TRUE.equals(request.isActive())) {
            throw new UserNotAdministratorException("Only group administrators can make group items active!");
        }
        GroupItemInShop groupItemInShop = groupItemInShopRepository.findById(groupItemId).orElseThrow(
                () -> new GroupItemInShopNotFoundException("Group item in shop with id: " + groupItemId + " not found!"));

        groupItemInShop.setPrice(request.price());
        groupItemInShop.setName(request.name());
        groupItemInShop.setIsActive(request.isActive());

        groupItemInShopRepository.save(groupItemInShop);
        return editGroupItemInShopMapper.toResponse(groupItemInShop);

    }
}
