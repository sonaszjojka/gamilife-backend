package pl.gamilife.gamification.domain.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.annotations.Immutable;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.shared.persistence.entity.BaseUuidReadOnlyEntity;

import java.util.UUID;

@Getter
@Entity
@Immutable
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "item", schema = "gamification")
@ToString(exclude = {"itemSlot", "rarity", "achievement", "unlockLevel"})
public class Item extends BaseUuidReadOnlyEntity {

    @Size(max = 150)
    @NotNull
    @Column(name = "name", nullable = false, length = 150)
    private String name;

    @Size(max = 255)
    @NotNull
    @Column(name = "description", nullable = false)
    private String description;

    @Size(max = 255)
    @NotNull
    @Column(name = "image_path", nullable = false)
    private String imagePath;

    @NotNull
    @Column(name = "quick_sell_value", nullable = false)
    private Integer quickSellValue;

    @NotNull
    @Column(name = "item_slot_id", nullable = false)
    private Integer itemSlotId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "item_slot_id", nullable = false, insertable = false, updatable = false)
    private ItemSlot itemSlot;

    @NotNull
    @Column(name = "rarity_id", nullable = false)
    private Integer rarityId;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "rarity_id", nullable = false, insertable = false, updatable = false)
    private Rarity rarity;

    @Column(name = "price")
    private Integer price;

    @Column(name = "achievement_id", insertable = false, updatable = false)
    private UUID achievementId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "achievement_id")
    private Achievement achievement;

    @Column(name = "unlock_level", insertable = false, updatable = false)
    private Integer unlockLevelId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "unlock_level")
    private Level unlockLevel;

    public ItemSlotEnum getItemSlotEnum() {
        return ItemSlotEnum.fromId(this.itemSlotId);
    }

    public RarityEnum getRarityEnum() {
        return RarityEnum.fromId(this.rarityId);
    }

    public boolean isForSale() {
        return price != null;
    }

}