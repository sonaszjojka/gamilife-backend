package edu.pjwstk.groupshop.usecase.editgroupshop;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupShopUseCaseImpl implements EditGroupShopUseCase {
    GroupShopRepository groupShopRepository;
    EditGroupShopMapper editGroupShopMapper;
    GroupApi groupApi;
    AuthApi authApi;

    public EditGroupShopUseCaseImpl(GroupShopRepository groupShopRepository, EditGroupShopMapper groupShopMapper, GroupApi groupApi, AuthApi authApi) {
        this.groupShopRepository = groupShopRepository;
        this.editGroupShopMapper = groupShopMapper;
        this.groupApi = groupApi;
        this.authApi = authApi;
    }

    @Override
    public EditGroupShopResponse execute(EditGroupShopRequest request, UUID shopId, UUID groupId) {

        CurrentUserDto currentUser = authApi.getCurrentUser();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(shopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + shopId + " not found!"));

        groupShop.setName(request.name());
        groupShop.setDescription(request.description());
        groupShopRepository.save(groupShop);

        return editGroupShopMapper.toResponse(groupShop);
    }
}
