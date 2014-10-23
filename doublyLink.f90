module class_DoublyLink
    implicit none
    private
    public :: DoublyLink
    public :: new_DoublyLink
    public :: toDoublyLink
    
    !Doubly Linked Listを構成する一つの要素
    type :: DoublyLink
        private
        class(*)         ,pointer :: value => null()
        class(DoublyLink),pointer :: prev  => null()
        class(DoublyLink),pointer :: next  => null()
    contains
        procedure,pass :: getValue    !class(*),pointer :: valueの値を取得
        procedure,pass :: setValue    !class(*),pointer :: valueの値を設定
        procedure,pass :: getNextLink !class(DoublyLink),pointer :: nextの値を取得
        procedure,pass :: setNextLink !class(DoublyLink),pointer :: nextの値を設定
        procedure,pass :: getPrevLink !class(DoublyLink),pointer :: prevの値を取得
        procedure,pass :: setPrevLink !class(DoublyLink),pointer :: prevの値を設定
        procedure,pass :: print       !class(*),pointer :: valueの値を画面表示
        final          :: destructDoublyLink !後始末
    end type DoublyLink
    
    !コンストラクタの名前をnew_DoublyLinkに設定
    interface new_DoublyLink
        procedure constructDoublyLink
    end interface
    
    !疑似的キャスト関数の名前をtoDoublyLinkに設定
    interface toDoublyLink
        procedure castToDoublyLinkPointer
    end interface
    
    contains
    
    !-----------------------------------------------------------------------------------------!
    !ヘルパ関数群
    !-----------------------------------------------------------------------------------------!
    !コンストラクタ（type-bound procedureではない）
    function constructDoublyLink(value,prevItem, nextItem) result(constructor)
        implicit none
        class(*)                 ,intent(in) :: value
        class(DoublyLink),pointer,intent(in) :: prevItem
        class(DoublyLink),pointer,intent(in) :: nextItem
        
        class(DoublyLink),pointer :: constructor
        
        !allocate and associate a new member in the list.
        allocate(DoublyLink::constructor)
        call constructor%setPrevLink(prevItem)
        call constructor%setNextLink(nextItem)
        
        !allocate list item depended on argument.
        call constructor%setValue(value)
        
    end function constructDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !デストラクタ(type-bound final procedure)
    subroutine destructDoublyLink(item)
        implicit none
        type(DoublyLink) :: item
        
        !print *,"finalize link item"
        call deleteValue(item)
        
    end subroutine destructDoublyLink
    !-----------------------------------------------------------------------------------------!
    !Listアイテムが持つLink以外の値を削除
    !-----------------------------------------------------------------------------------------!
    subroutine deleteValue(this)
        implicit none
        class(DoublyLink),intent(inout) :: this
        if(associated(this%value))then
            
            !print *,associated(this%value)
            deallocate(this%value)
        end if
    end subroutine deleteValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !データ表示（整数，文字，浮動小数のみを表示．
    !特に意味はないので削除を推奨
    subroutine print(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        
        select type(value=>this%value)
            type is (integer)
                print *,value
            type is (character(*))
                print *,value
            type is (real)
                print *,value
            class default
                stop 'printLink : unexepted type for link'
        end select
        
    end subroutine print
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !class(*)で定義されているListの値がDoublyLink型ならその値へのポインタを返す
    !擬似的なキャストとしての使用を想定
    function castToDoublyLinkPointer(value) result(linkItem)
        implicit none
        class(*),target :: value

        class(DoublyLink),pointer :: linkItem
        
        select type(value)
        class is(DoublyLink); linkItem => value
        end select
        
    end function castToDoublyLinkPointer
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !次のList要素のセッターとゲッター
    !-----------------------------------------------------------------------------------------!
    !次のList要素へのポインタを取得
    function getNextLink(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        
        class(DoublyLink),pointer :: getNextLink
        
        getNextLink => this%next
        
    end function getNextLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !次のList要素へのポインタを設定
    subroutine setNextLink(this, next)
        implicit none
        class(DoublyLink),intent(inout) :: this
        class(DoublyLink),pointer,intent(in) :: next
        
        this%next => next
    end subroutine setNextLink
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !List Itemの値のセッターとゲッター
    !-----------------------------------------------------------------------------------------!
    !値へのポインタを取得
    function getValue(this) result(value)
        implicit none
        class(DoublyLink),intent(in) :: this
        class(*),pointer :: value
        
        value => this%value
    end function getValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !値へのポインタを設定
    subroutine setValue(this, value)
        implicit none
        class(DoublyLink),intent(inout) :: this
        class(*)         ,intent(in) :: value
        
        allocate(this%value, source=value)
    end subroutine setValue
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !前のList要素のセッターとゲッター
    !-----------------------------------------------------------------------------------------!
    !前のList要素へのポインタを取得
    function getPrevLink(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        class(DoublyLink),pointer    :: getPrevLink
        
        getPrevLink => this%prev
        
    end function getPrevLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !前のList要素へのポインタを設定
    subroutine setPrevLink(this, prev)
        implicit none
        class(DoublyLink),        intent(inout) :: this
        class(DoublyLink),pointer,intent(in   ) :: prev
        
        this%prev => prev
    end subroutine setPrevLink
    !-----------------------------------------------------------------------------------------!
    
end module class_DoublyLink
