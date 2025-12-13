package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.changeGroupShopStatus.ChangeGroupShopStatusRequest;
import pl.gamilife.groupshop.application.changeGroupShopStatus.ChangeGroupShopStatusResponse;
import pl.gamilife.groupshop.application.changeGroupShopStatus.ChangeGroupShopStatusUseCase;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopRequest;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopResponse;
import pl.gamilife.groupshop.application.editgroupshop.EditGroupShopUseCase;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {
    private final ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase;
    private final EditGroupShopUseCase editGroupShopUseCase;


    public GroupShopController(ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase, EditGroupShopUseCase editGroupShopUseCase) {
        this.changeGroupShopStatusUseCase = changeGroupShopStatusUseCase;
        this.editGroupShopUseCase = editGroupShopUseCase;
    }


    @PutMapping("/{shopId}")
    public ResponseEntity<EditGroupShopResponse> editGroupShop(@PathVariable(name = "groupId") UUID groupId,
                                                               @PathVariable(name = "shopId") UUID shopId,
                                                               @RequestBody EditGroupShopRequest request) {

        EditGroupShopResponse response = editGroupShopUseCase.execute(request, shopId, groupId);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{shopId}/status")
    public ResponseEntity<ChangeGroupShopStatusResponse> changeGroupShopStatus(@PathVariable(name = "groupId") UUID groupId,
                                                                               @PathVariable(name = "shopId") UUID shopId,
                                                                               @RequestBody @Valid ChangeGroupShopStatusRequest request) {
        ChangeGroupShopStatusResponse response = changeGroupShopStatusUseCase.execute(request, shopId, groupId);
        return ResponseEntity.ok(response);
    }
}
