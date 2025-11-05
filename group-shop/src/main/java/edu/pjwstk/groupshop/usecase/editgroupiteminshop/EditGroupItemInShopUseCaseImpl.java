package edu.pjwstk.groupshop.usecase.editgroupiteminshop;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.exception.common.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupItemInShopNotFoundException;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.InactiveGroupShopException;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupItemInShopUseCaseImpl implements EditGroupItemInShopUseCase {
    private final EditGroupItemInShopMapper editGroupItemInShopMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final AuthApi currentUserProvider;
    private final GroupApi groupApi;

    public EditGroupItemInShopUseCaseImpl(EditGroupItemInShopMapper editGroupItemInShopMapper, GroupItemInShopRepository groupItemInShopRepository, GroupShopRepository groupShopRepository, AuthApi currentUserProvider, GroupApi groupApi) {
        this.editGroupItemInShopMapper = editGroupItemInShopMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.groupShopRepository = groupShopRepository;
        this.currentUserProvider = currentUserProvider;
        this.groupApi = groupApi;
    }

    @Override
    public EditGroupItemInShopResponse execute(UUID groupItemId, UUID groupId, EditGroupItemInShopRequest request) {

        GroupShop groupShop = groupShopRepository.findByGroupId(groupId).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        GroupDto groupDto = groupApi.findGroupById(groupId);

        if (!currentUserDto.userId().equals(groupDto.adminId()) && Boolean.TRUE.equals(request.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can make group items active!");
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
