package edu.pjwstk.groupshop.usecase.creategroupiteminshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CreateGroupItemInShopUseCaseImpl implements CreateGroupItemInShopUseCase {
    private final CreateGroupItemInShopMapper createGroupItemInShopMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final AuthApi currentUserProvider;

    public CreateGroupItemInShopUseCaseImpl(CreateGroupItemInShopMapper createGroupItemInShopMapper, GroupItemInShopRepository groupItemInShopRepository, GroupShopRepository groupShopRepository, GroupApi groupProvider, AuthApi currentUserProvider) {
        this.createGroupItemInShopMapper = createGroupItemInShopMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.groupShopRepository = groupShopRepository;
        this.groupProvider = groupProvider;
        this.currentUserProvider = currentUserProvider;
    }


    @Override
    public CreateGroupItemInShopResponse execute(CreateGroupItemInShopRequest request, UUID groupId, UUID groupShopId) {

        GroupShop groupShop = groupShopRepository.findByGroupShopId(groupShopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + groupShopId + " not found!"));

        GroupDto groupDto = groupProvider.findGroupById(groupId);
        if (!groupShop.getGroupId().equals(groupId))
        {
            throw new GroupShopNotFoundException("Group shop with id: " + groupShopId + " does not belong to group with id: " + groupId + "!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser()
                .orElseThrow();

        if (!currentUserDto.userId().equals(groupDto.adminId())&& Boolean.TRUE.equals(request.isActive())) {
            throw new UserNotAdministratorException("Only group administrators can create active group item in shop!");
        }
        GroupItemInShop groupItemInShop = createGroupItemInShopMapper.toEntity(request, groupShop, UUID.randomUUID());
        groupItemInShopRepository.save(groupItemInShop);
        return createGroupItemInShopMapper.toResponse(groupItemInShop);
    }
}
