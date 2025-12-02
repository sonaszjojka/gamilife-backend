package edu.pjwstk.groupshop.usecase.creategroupiteminshop;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.domain.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.domain.InactiveGroupShopException;
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

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        GroupDto groupDto = groupProvider.findGroupById(groupId);
        if (!groupShop.getGroupId().equals(groupId)) {
            throw new GroupShopNotFoundException("Group shop with id: " + groupShopId + " does not belong to group with id: " + groupId + "!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

        if (!currentUserDto.userId().equals(groupDto.adminId()) && Boolean.TRUE.equals(request.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create active group item in shop!");
        }
        GroupItemInShop groupItemInShop = createGroupItemInShopMapper.toEntity(request, groupShop, UUID.randomUUID());
        groupItemInShopRepository.save(groupItemInShop);
        return createGroupItemInShopMapper.toResponse(groupItemInShop);
    }
}
