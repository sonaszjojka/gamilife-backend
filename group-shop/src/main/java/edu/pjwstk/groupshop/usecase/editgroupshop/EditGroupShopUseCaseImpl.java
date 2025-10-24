package edu.pjwstk.groupshop.usecase.editgroupshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import edu.pjwstk.groupshop.shared.ApiResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class EditGroupShopUseCaseImpl implements EditGroupShopUseCase {
    GroupShopRepository groupShopRepository;
    EditGroupShopMapper editGroupShopMapper;
    GroupApi groupApi;
    AuthApi authApi;

    public EditGroupShopUseCaseImpl(GroupShopRepository groupShopRepository,EditGroupShopMapper groupShopMapper, GroupApi groupApi, AuthApi authApi) {
        this.groupShopRepository = groupShopRepository;
        this.editGroupShopMapper = groupShopMapper;
        this.groupApi = groupApi;
        this.authApi = authApi;
    }

    @Override
    public EditGroupShopResponse execute(EditGroupShopRequest request, UUID shopId, UUID groupId) {

        CurrentUserDto currentUser = authApi.getCurrentUser().orElseThrow();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new RuntimeException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(shopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + shopId + " not found!"));

        groupShop.setName(request.name());
        groupShop.setDescription(request.description());
        groupShopRepository.save(groupShop);

        return editGroupShopMapper.toResponse(groupShop);
    }
}
